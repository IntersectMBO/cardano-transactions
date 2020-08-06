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
    , Network (..)

   -- * Constructing Primitives
    , mkInput
    , mkOutput

    -- Internal
    , Shelley
    ) where

import Cardano.Api.Typed
    ( TxIn (..), TxOut (..) )
import Cardano.Crypto.Hash.Class
    ( Hash (UnsafeHash) )
import Data.ByteString
    ( ByteString )
import Data.ByteString.Short
    ( toShort )
import Data.UTxO.Transaction
    ( MkPayment (..) )
import Data.Word
    ( Word32 )
import Numeric.Natural
    ( Natural )
import Shelley.Spec.Ledger.BaseTypes
    ( Network (..) )

import qualified Cardano.Api.Typed as Cardano
import qualified Data.ByteString as BS

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

    empty :: Network -> CoinSel Shelley
    empty net = (net, mempty, mempty)

    addInput :: TxIn -> CoinSel Shelley -> CoinSel Shelley
    addInput inp (pm, inps, outs) = (pm, inp : inps, outs)

    addOutput :: TxOut Cardano.Shelley -> CoinSel Shelley -> CoinSel Shelley
    addOutput out (pm, inps, outs) = (pm, inps, out : outs)

    lock = undefined
    signWith = undefined
    serialize = undefined

-- | Construct a payment 'Input' for /Shelley/ from primitive types.
--
-- __example__:
--
-- >>> mkInput 14 =<< fromBase16 "3b402651...aad1c0b7"
-- Just (Input ...)
--
-- @since 2.0.0
mkInput
    :: Word32
        -- ^ Input index.
    -> ByteString
        -- ^ Input transaction id. See also: 'fromBase16'.
    -> Maybe (Input Shelley)
mkInput ix bytes =
    if BS.length bytes == 32 then
        Just $ Cardano.TxIn
            (Cardano.TxId $ UnsafeHash $ toShort bytes)
            (Cardano.TxIx (fromIntegral ix))
    else
        Nothing

-- | Construct a payment 'Output' for /Shelley/ from primitive types.
--
-- __example__:
--
-- >>> mkOutput 42 =<< fromBase58 "Ae2tdPwU...DnXy319f"
-- >>> mkOutput 42 =<< fromBech32 "addr1sjc...6s3xvu5g"
-- >>> mkOutput 42 =<< fromBase16 "42bf330c...ba5b947e"
-- Just (Output ...)
--
-- @since 2.0.0
mkOutput
    :: Natural
        -- ^ Output value, in Lovelace (1 Ada = 1e6 Lovelace).
    -> ByteString
        -- ^ Output Address. See also: 'fromBase58', 'fromBase16', 'fromBech32'.
    -> Maybe (Output Shelley)
mkOutput coin bytes =
    Cardano.deserialiseFromRawBytes Cardano.AsShelleyAddress bytes >>=
    (\addr -> pure $ Cardano.TxOut addr (Cardano.Lovelace $ fromIntegral coin) )
