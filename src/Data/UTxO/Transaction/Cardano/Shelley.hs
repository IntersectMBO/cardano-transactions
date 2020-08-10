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
    , NetworkId (..)

   -- * Constructing Primitives
    , mkInput
    , mkOutput
    , mkShelleySignKey

    -- Internal
    , Shelley
    ) where

import Cardano.Api.Typed
    ( NetworkId, TxExtraContent (..), TxIn (..), TxOut (..) )
import Cardano.Crypto.Hash.Class
    ( Hash (UnsafeHash) )
import Cardano.Crypto.Wallet
    ( xprv )
import Cardano.Slotting.Slot
    ( SlotNo (..) )
import Data.ByteString
    ( ByteString )
import Data.ByteString.Short
    ( toShort )
import Data.Either.Extra
    ( eitherToMaybe )
import Data.List.NonEmpty
    ( NonEmpty )
import Data.UTxO.Transaction
    ( ErrMkPayment (..), MkPayment (..) )
import Data.Word
    ( Word32, Word64 )
import Numeric.Natural
    ( Natural )

import qualified Cardano.Api.Typed as Cardano
import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE

-- | Construct a payment 'Init' for /Shelley/ from primitive types.
--
-- __examples__:
--
-- >>> mkInit Mainnet 430000
-- >>> mkInit (Testnet (NetworkMagic 1234)) 430000
--
-- @since 2.0.0
mkInit
    :: NetworkId
        -- ^ A network tag, Mainnet or Testnet with NetworkMagic specified
    -> Word64
        -- ^ A ttl expressed in slot number counted from the beginning of blockchain
    -> Init Shelley
mkInit net ttl = (net, SlotNo ttl)

--
-- MkPayment instance
--

-- Type-level constructor capturing types for 'Shelley'.
data Shelley

instance MkPayment Shelley where
    type Init Shelley = (NetworkId, SlotNo)

    type Input   Shelley = TxIn
    type Output  Shelley = TxOut Cardano.Shelley
    type SignKey Shelley = Cardano.ShelleyWitnessSigningKey

    type CoinSel Shelley =
        (NetworkId, SlotNo, [TxIn], [TxOut Cardano.Shelley])

    type Tx Shelley = Either
        ErrMkPayment
        ( NetworkId
        , NonEmpty TxIn
        , NonEmpty (TxOut Cardano.Shelley)
        , Cardano.TxBody Cardano.Shelley
        , [Cardano.Witness Cardano.Shelley]
        )

    empty :: Init Shelley -> CoinSel Shelley
    empty (net, ttl) = (net, ttl, mempty, mempty)

    addInput :: TxIn -> CoinSel Shelley -> CoinSel Shelley
    addInput inp (pm, ttl, inps, outs) = (pm, ttl, inp : inps, outs)

    addOutput :: TxOut Cardano.Shelley -> CoinSel Shelley -> CoinSel Shelley
    addOutput out (pm, ttl, inps, outs) = (pm, ttl, inps, out : outs)

    lock :: CoinSel Shelley -> Tx Shelley
    lock (_net, _ttl, [], _outs) = Left MissingInput
    lock (_net, _ttl, _inps, []) = Left MissingOutput
    lock (net, ttl, inps, outs) =
        Right (net, neInps, neOuts, sigData, mempty)
      where
        sigData = Cardano.makeShelleyTransaction
            TxExtraContent
                { txMetadata = Nothing
                , txWithdrawals = []
                , txCertificates = []
                , txUpdateProposal = Nothing
                }
            ttl
            (Cardano.Lovelace 250000) -- here I need to use Cardano.feeCalculation or Cardano.feeEstimation
            inps
            outs
        neInps  = NE.fromList $ reverse inps
        neOuts  = NE.fromList $ reverse outs

    signWith :: SignKey Shelley -> Tx Shelley -> Tx Shelley
    signWith _ (Left e) = Left e
    signWith signingKey (Right (net, inps, outs, sigData, wits)) =
        Right (net, inps, outs, sigData, shelleyWit : wits)
      where
        shelleyWit = Cardano.makeShelleyKeyWitness sigData signingKey

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


-- | Construct a 'SignKey' for /Shelley/ from primitive types.
--
-- __example__:
--
-- >>> mkShelleySignKey =<< fromBech32 "xprv13f0ve...nu4v4h875l"
-- Just (SignKey ...)
--
-- @since 1.0.0
mkShelleySignKey
    :: ByteString
        -- ^ A extended address private key and its chain code.
        -- The key __must be 96 bytes__ long, internally made of two concatenated parts:
        --
        -- @
        -- BYTES = PRV | CC
        -- PRV   = 64OCTET  # a 64 bytes Ed25519 extended private key
        -- CC    = 32OCTET  # a 32 bytes chain code
        -- @
        --
        -- See also: 'fromBech32'.
    -> Maybe (SignKey Shelley)
mkShelleySignKey bytes
    | BS.length bytes /= 96 = Nothing
    | otherwise =
        fmap (Cardano.WitnessPaymentExtendedKey . Cardano.PaymentExtendedSigningKey)
        $ eitherToMaybe $ xprv bytes
