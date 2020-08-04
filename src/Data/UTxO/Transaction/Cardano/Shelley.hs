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

import Cardano.Api.Typed
    ( TxIn (..), TxOut (..) )
import Data.UTxO.Transaction
    ( MkPayment (..) )
import Shelley.Spec.Ledger.BaseTypes
    ( Network (..) )

import qualified Cardano.Api.Typed as Cardano

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
    type Output  Shelley = TxOut Cardano.Shelley
    type SignKey Shelley = ()

    type CoinSel Shelley =
        (Network, [TxIn], [TxOut Cardano.Shelley])

    type Tx Shelley = ()

    empty = undefined
    addInput = undefined
    addOutput = undefined
    lock = undefined
    signWith = undefined
    serialize = undefined
