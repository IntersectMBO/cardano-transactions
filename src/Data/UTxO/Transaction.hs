{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Data.UTxO.Transaction
    (
    -- * Overview
    -- $overview

    -- * Abstraction
      MkPayment (..)
    , ErrMkPayment (..)

    -- * Note about type-level constraints
    -- $note
    ) where

import Prelude

import Data.ByteString
    ( ByteString )

-- $overview
--
-- This module offers an abstraction over the construction of simple UTxO
-- transactions. Such a transaction is called a /Payment/. Payments are
-- constructed by selecting inputs and outputs, and signing each input
-- independently.
--
-- This can be represented as a state machine, such as:
--
-- @
--                          empty
--                            |
--      *-----------------*   |   *----------------*
--      |                 |   |   |                |
--      |                 v   v   v                |
--      *--- addOutput ---=========--- addInput ---*
--                            |
--                            |
--                          lock  *----------------*
--                            |   |                |
--                            |   v                |
--                        =========--- signWith ---*
--                            |
--                            |
--                        serialize
--                            |
--                            |
--                            v
--                           ...
-- @
--
-- Functions from the 'MkPayment' interfaces are designed to be easy to chain
-- like pipes. To give a concrete example, let's see how to construct a
-- transaction with /one input/ and /two outputs/:
--
-- @
-- import Data.Function
--     ( (&) )
-- import qualified Data.UTxO.Transaction as Tx
--
-- bytes = Tx.empty
--       & Tx.addInput  input0
--       & Tx.addOutput output0
--       & Tx.addOutput output1
--       & Tx.lock
--       & Tx.signWith  key0
--       & Tx.serialize
-- @
--
-- The reason why the 'lock' step is required is because, in order to create
-- witnesses, one must produce signatures of the whole transaction body (inputs
-- + outputs). Hence, signatures can only be added once all inputs and all
-- outputs have already been addded.
--
-- This is correct-by-construction thanks to the 'lock' step and the distinction
-- between 'CoinSel' and 'Tx'.

-- | Abstract interface for constructing a /Payment/.
class MkPayment dom where
    -- | Some initial extra data needed to construct and or serialize the
    -- transaction. This is a free-type which is implementation-specific.
    --
    -- For example, Byron transactions requires the underlying protocol magic in
    -- order to produce witnesses. This is an appropriate place for.
    type Init    dom :: *

    -- | Type representation of transaction 'Input'
    type Input   dom = inp | inp -> dom

    -- | Type representation of transaction 'Output'
    type Output  dom = out | out -> dom

    -- | Type representation of transaction 'SignKey'
    type SignKey dom = key | key -> dom

    -- | An intermediate type for representing a transaction under construction.
    -- 'Input' and 'Output' can still be added to a 'CoinSel' and must be added
    -- before trying to sign any of the inputs.
    type CoinSel dom = sel | sel -> dom

    -- | A transaction is a locked 'CoinSel' to which it is no longer possible
    -- to add new 'Input' and or 'Output'. Only 'Tx' can be serialized.
    type Tx      dom = tx  | tx  -> dom

    -- * Transaction construction
    empty     :: Init dom -> CoinSel dom
    addInput  :: Input  dom -> CoinSel dom -> CoinSel dom
    addOutput :: Output dom -> CoinSel dom -> CoinSel dom

    -- * Transaction signature
    lock      :: CoinSel dom -> Tx dom
    signWith  :: SignKey dom -> Tx dom -> Tx dom

    -- * Transaction serialization
    serialize :: Tx dom -> Either ErrMkPayment ByteString

-- | Possible errors occuring when constructing a /Payment/. This can in
-- practice be safely ignored if the transaction is known to be well-formed.
data ErrMkPayment
    = MissingInput
        -- ^ Payments must have at least one input.
    | MissingOutput
        -- ^ Payments must have at least one output
    | MissingSignature
        -- ^ Payments must have a signature for each input.
    deriving (Show, Read, Eq)

-- $note
--
-- It is relatively easy to tweak the interface below to get it fully type-safe
-- by using an inductive type, such as:
--
-- @
-- data N = Zero | Some N
--
-- class MkPayment dom where
--     type Init    dom :: *
--
--     type Input   dom = inp | inp -> dom
--     type Output  dom = out | out -> dom
--     type SignKey dom = key | key -> dom
--
--     type CoinSel dom = (sel :: N -> N -> *) | sel -> dom
--     type Tx      dom = (tx  :: N -> *)      | tx  -> dom
--
--     empty     :: Init dom -> CoinSel dom Zero Zero
--
--     addInput  :: Input  dom -> CoinSel dom n m -> CoinSel dom (Some n) m
--     addOutput :: Output dom -> CoinSel dom n m -> CoinSel dom n (Some m)
--
--     lock      :: CoinSel dom (Some n) (Some m) -> Tx dom (Some n)
--
--     signWith :: SignKey dom -> Tx dom (Some n) -> Tx dom n
--
--     serialize :: Tx dom Zero -> ByteString
-- @
--
-- Such approach is great for ensuring that the transaction can't be ill-formed
-- and works well when manually constructing the transaction as shown above.
--
-- It gets however quite complex when one wants to start folding or mapping on a
-- list of inputs or outputs where their numbers isn't known at compile-time!
-- Indeed, each call to 'addInput' does actually return a new type!
--
-- Let's say we want to write the following function:
--
-- @
-- addMany :: [Input dom] -> CoinSel dom n m -> CoinSel dom ? m
-- @
--
-- What should be the type of `?`. This can't possibly known at compile-time.
