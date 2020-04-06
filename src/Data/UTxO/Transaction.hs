{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Data.UTxO.Transaction
    ( UTxOTransaction (..)
    , N(..)
    ) where

import Prelude

import Data.ByteString
    ( ByteString )
import Data.Word
    ( Word32 )
import Numeric.Natural
    ( Natural )

-- | Construct a UTxO Transaction from inputs and outputs. The interface is
-- pretty generic and may be a bit _frightening_ but it's in essence very simple
-- and ensure that the transaction is correct-by-construction.
--
-- For instance, constructing a transaction with one input and two inputs is as
-- simple as:
--
-- @
-- let transaction = empty
--          & addInput   inp0
--          & addOutput  out0
--          & addOutput  out1
--          & lock
--          & addWitness wit0
--          & serialize
-- @
--
-- The nice thing about this interface is that it lets client choose underlying
-- concrete types which remove a lot of the complexity AND, it prevents the
-- serialization of ill-formed transactions with missing inputs.
class UTxOTransaction dom where
    type Init    dom :: *

    type Input   dom = inp | inp -> dom
    type Output  dom = out | out -> dom
    type SignKey dom = key | key -> dom

    type CoinSel dom = (sel :: N -> N -> *) | sel -> dom
    type Tx      dom = (tx  :: N -> *)      | tx  -> dom

    -- * Primitive types construction
    mkInput   :: ByteString -> Word32  -> Maybe (Input dom)
    mkOutput  :: ByteString -> Natural -> Maybe (Output dom)
    mkSignKey :: ByteString -> Maybe (SignKey dom)

    -- * Transaction construction
    empty     :: Init dom   -> CoinSel dom Zero Zero
    addInput  :: Input dom  -> CoinSel dom n m -> CoinSel dom (Some n) m
    addOutput :: Output dom -> CoinSel dom n m -> CoinSel dom n (Some m)

    -- * Transaction signature
    lock       :: CoinSel dom (Some n) (Some m) -> Tx dom (Some n)
    addWitness :: SignKey dom -> Tx dom (Some n) -> Tx dom n

    -- * Transaction serialization
    serialize :: Tx dom Zero -> ByteString

-- | An inductive type for counting inputs  & signatures.
data N = Zero | Some N
