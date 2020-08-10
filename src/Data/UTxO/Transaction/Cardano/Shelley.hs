{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
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
    , mkByronSignKey

    -- Internal
    , Shelley
    ) where


import Cardano.Api.Typed
    ( NetworkId, TxExtraContent (..), TxIn (..), TxOut (..) )
import Cardano.Binary
    ( serialize' )
import Cardano.Crypto.Hash.Class
    ( Hash (UnsafeHash), hashWith )
import Cardano.Crypto.Signing
    ( SigningKey (..) )
import Cardano.Crypto.Wallet
    ( xprv )
import Cardano.Slotting.Slot
    ( SlotNo (..) )
import Control.Monad
    ( replicateM )
import Control.Monad.Fail
    ( MonadFail )
import Data.ByteString
    ( ByteString )
import Data.ByteString.Short
    ( toShort )
import Data.Either.Extra
    ( eitherToMaybe )
import Data.UTxO.Transaction
    ( ErrMkPayment (..), MkPayment (..) )
import Data.UTxO.Transaction.Cardano.Helpers
    ( ed25519ScalarMult )
import Data.Word
    ( Word32, Word64 )
import Numeric.Natural
    ( Natural )

import qualified Cardano.Api.Typed as Cardano
import qualified Cardano.Chain.Common as Byron
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Ouroboros.Consensus.Shelley.Ledger as Ouroboros
import qualified Shelley.Spec.Ledger.Address.Bootstrap as Bootstrap

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

type ByronSigningKey = (ByteString, SigningKey)

instance MkPayment Shelley where
    type Init Shelley = (NetworkId, SlotNo)

    type Input   Shelley = TxIn
    type Output  Shelley = TxOut Cardano.Shelley
    type SignKey Shelley = Either ByronSigningKey Cardano.ShelleyWitnessSigningKey

    type CoinSel Shelley =
        (NetworkId, SlotNo, [TxIn], [TxOut Cardano.Shelley])

    type Tx Shelley = Either
        ErrMkPayment
        ( NetworkId
        , [TxIn]
        , [TxOut Cardano.Shelley]
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
        Right (net, inps', outs', sigData, mempty)
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
            inps'
            outs'
        inps'  = reverse inps
        outs'  = reverse outs

    signWith :: SignKey Shelley -> Tx Shelley -> Tx Shelley
    signWith _ (Left e) = Left e
    signWith (Right signingKey) (Right (net, inps, outs, sigData, wits)) =
        Right (net, inps, outs, sigData, shelleyWit : wits)
      where
        shelleyWit = Cardano.makeShelleyKeyWitness sigData signingKey
    signWith (Left (addr, signingKey)) (Right (net, inps, outs, sigData, wits)) =
        Right (net, inps, outs, sigData, byronWit : wits)
      where
        byronWit = Cardano.ShelleyBootstrapWitness $
            Bootstrap.makeBootstrapWitness txHash signingKey addrAttr
        (Cardano.ShelleyTxBody body _) = sigData
        txHash = hashWith serialize' body
        addrAttr = Byron.mkAttributes $ Byron.AddrAttributes
            (toHDPayloadAddress addr)
            (Cardano.toByronNetworkMagic net)

    serialize :: Tx Shelley -> Either ErrMkPayment ByteString
    serialize (Left e) = Left e
    serialize (Right (_net, inps, _outs, sigData, wits))
        | length inps /= length wits = Left MissingSignature
        | otherwise = Right $ serialize' $ Ouroboros.mkShelleyTx tx
      where
        (Cardano.ShelleyTx tx) = Cardano.makeSignedTransaction wits sigData

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
-- This is for Shelley era keys.
--
-- __example__:
--
-- >>> mkShelleySignKey =<< fromBech32 "xprv13f0ve...nu4v4h875l"
-- Just (SignKey ...)
--
-- @since 2.0.0
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
        fmap (Right . Cardano.WitnessPaymentExtendedKey . Cardano.PaymentExtendedSigningKey)
        $ eitherToMaybe $ xprv bytes

-- | Construct a 'SignKey' for /Shelley/ from primitive types.
-- This is for Byron era keys.
--
-- __example__:
--
-- >>> let (Just addr) = frombase58 "DdzFFzCqrh...Dwg3SiaHiEL"
-- >>> mkByronSignKey addr =<< fromBech32 "xprv13f0ve...nu4v4h875l"
-- Just (SignKey ...)
--
-- @since 2.0.0
mkByronSignKey
    :: ByteString
        -- ^ Address derived from extended priate key below
        -- See also: 'fromBase58'.
    -> ByteString
        -- ^ A extended address private key and its chain code.
        -- The key __must be 96 bytes__ long, internally made of two concatenated parts:
        --
        -- @
        -- BYTES = PRV | CC
        -- PRV   = 64OCTET  # a 64 bytes Ed25519 extended private key
        -- CC    = 32OCTET  # a 32 bytes chain code
        -- @
        --
        -- See also: 'fromBase16'.
    -> Maybe (SignKey Shelley)
mkByronSignKey bytes addr
    | BS.length bytes /= 96 && BS.length addr /= 76 = Nothing
    | otherwise = do
        let (prv, cc) = BS.splitAt 64 bytes
        pub <- ed25519ScalarMult (BS.take 32 prv)
        fmap (Left . (addr,) . SigningKey) $ eitherToMaybe $ xprv $ prv <> pub <> cc

toHDPayloadAddress :: ByteString -> Maybe Byron.HDAddressPayload
toHDPayloadAddress addr = do
    payload <- deserialiseCbor decodeAddressPayload addr
    attributes <- deserialiseCbor decodeAllAttributes' payload
    case filter (\(tag,_) -> tag == 1) attributes of
        [(1, bytes)] ->
            Byron.HDAddressPayload <$> decodeNestedBytes CBOR.decodeBytes bytes
        _ ->
            Nothing
  where
    decodeAllAttributes' = do
        _ <- CBOR.decodeListLenCanonicalOf 3
        _ <- CBOR.decodeBytes
        decodeAllAttributes

    decodeAllAttributes = do
        n <- CBOR.decodeMapLenCanonical -- Address Attributes length
        replicateM n decodeAttr
      where
        decodeAttr = (,) <$> CBOR.decodeWord8 <*> CBOR.decodeBytes

    decodeAddressPayload = do
        _ <- CBOR.decodeListLenCanonicalOf 2
        _ <- CBOR.decodeTag
        bytes <- CBOR.decodeBytes
        _ <- CBOR.decodeWord32 -- CRC
        return bytes

    decodeNestedBytes
        :: MonadFail m
        => (forall s. CBOR.Decoder s r)
        -> ByteString
        -> m r
    decodeNestedBytes dec bytes =
        case CBOR.deserialiseFromBytes dec (BL.fromStrict bytes) of
            Right ("", res) ->
                pure res
            Right _ ->
                fail "Leftovers when decoding nested bytes"
            _ ->
                fail "Could not decode nested bytes"

    deserialiseCbor
        :: (forall s. CBOR.Decoder s a)
        -> ByteString
        -> Maybe a
    deserialiseCbor dec =
        fmap snd . eitherToMaybe . CBOR.deserialiseFromBytes dec . BL.fromStrict
