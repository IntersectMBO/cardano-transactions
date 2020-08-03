{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_HADDOCK prune #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-} -- needed for {#- HLINT ... #-}

module Data.UTxO.Transaction.Cardano.Shelley
    (
    -- * Initialization
      mkInit
    , mainnetMagic
    , testnetMagic

    -- Internal
    , Shelley
    ) where

import Cardano.Chain.UTxO
    ( TxIn (..), TxInWitness (..), TxOut (..), TxSigData (..) )
import Cardano.Crypto.Signing
    ( SigningKey (..) )
import Data.List.NonEmpty
    ( NonEmpty )
import Data.UTxO.Transaction
    ( ErrMkPayment (..), MkPayment (..) )
import Shelley.Spec.Ledger.BaseTypes
    ( Network (..) )


-- | Construct a payment 'Init' for /Shelley/ from primitive types.
--
-- __examples__:
--
-- >>> mkInit Mainnet == mainnetMagic
-- True
--
-- >>> mkInit Testnet == testnetMagic
-- True
--
-- @since 2.0.0
mkInit
    :: Network
        -- ^ A network tag
    -> Init Shelley
mkInit net = net

-- | Pre-defined 'Init' magic for /Shelley/ MainNet.
--
-- @since 2.0.0
mainnetMagic :: Init Shelley
mainnetMagic = mkInit Mainnet

-- | Pre-defined 'Init' magic for /Shelley/ TestNet.
--
-- @since 2.0.0
testnetMagic :: Init Shelley
testnetMagic = mkInit Testnet

--
-- MkPayment instance
--

-- Type-level constructor capturing types for 'Shelley'.
data Shelley

instance MkPayment Shelley where
    type Init Shelley = Network

    type Input   Shelley = TxIn
    type Output  Shelley = TxOut
    type SignKey Shelley = SigningKey

    type CoinSel Shelley =
        (Network, [TxIn], [TxOut])

    type Tx Shelley = Either
        ErrMkPayment
        (Network, NonEmpty TxIn, NonEmpty TxOut, TxSigData, [TxInWitness])

    empty = undefined
    addInput = undefined
    addOutput = undefined
    lock = undefined
    signWith = undefined
    serialize = undefined
