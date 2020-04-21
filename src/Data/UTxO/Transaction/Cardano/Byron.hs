{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_HADDOCK prune #-}

module Data.UTxO.Transaction.Cardano.Byron
    (
    -- * Initialization
      mkInit
    , mainnetMagic
    , testnetMagic

    -- * Constructing Primitives
    , mkInput
    , mkOutput
    , mkSignKey

    -- * Converting From Bases
    , fromBase16
    , fromBase58
    , fromBase64

    -- Internal
    , Byron
    , encodeCoinSel
    , decodeCoinSel
    , encodeTx
    , decodeTx
    ) where

import Cardano.Binary
    ( FromCBOR (..), ToCBOR (..) )
import Cardano.Chain.Common
    ( mkAttributes, mkLovelace )
import Cardano.Chain.UTxO
    ( TxIn (..), TxInWitness (..), TxOut (..), TxSigData (..), mkTxAux )
import Cardano.Crypto.Hashing
    ( AbstractHash (..), hash )
import Cardano.Crypto.ProtocolMagic
    ( ProtocolMagicId (..) )
import Cardano.Crypto.Signing
    ( SignTag (..), Signature, SigningKey (..), VerificationKey (..) )
import Cardano.Crypto.Wallet
    ( toXPub, xprv )
import Codec.CBOR.Read
    ( deserialiseFromBytes )
import Crypto.Error
    ( eitherCryptoError )
import Crypto.Hash
    ( Blake2b_256, digestFromByteString )
import Data.ByteArray.Encoding
    ( Base (..), convertFromBase )
import Data.ByteString
    ( ByteString )
import Data.ByteString.Base58
    ( bitcoinAlphabet, decodeBase58 )
import Data.Either.Extra
    ( eitherToMaybe )
import Data.List.NonEmpty
    ( NonEmpty, nonEmpty )
import Data.Text
    ( Text )
import Data.UTxO.Transaction
    ( ErrMkPayment (..), MkPayment (..) )
import Data.Word
    ( Word32 )
import GHC.Exts
    ( IsList (fromList) )
import Numeric.Natural
    ( Natural )

import qualified Cardano.Chain.UTxO as CC
import qualified Cardano.Crypto.Signing as CC
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Crypto.ECC.Edwards25519 as Ed25519
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- | Construct a payment 'Init' for /Byron/ from primitive types.
--
-- __examples__:
--
-- >>> mkInit 764824073 == mainnetMagic
-- True
--
-- >>> mkInit 1097911063 == testnetMagic
-- True
--
-- @since 1.0.0
mkInit
    :: Word32
        -- ^ A protocol magic id
    -> Init Byron
mkInit =
    ProtocolMagicId

-- | Pre-defined 'Init' magic for /Byron/ MainNet.
--
-- @since 1.0.0
mainnetMagic :: Init Byron
mainnetMagic = mkInit 764824073

-- | Pre-defined 'Init' magic for /Byron/ TestNet.
--
-- @since 1.0.0
testnetMagic :: Init Byron
testnetMagic = mkInit 1097911063

-- | Construct a payment 'Input' for /Byron/ from primitive types.
--
-- __example__:
--
-- >>> mkInput 14 =<< fromBase16 "3b402651...aad1c0b7"
-- Just (Input ...)
--
-- @since 1.0.0
mkInput
    :: Word32
        -- ^ Input index.
    -> ByteString
        -- ^ Input transaction id. See also: 'fromBase16'.
    -> Maybe (Input Byron)
mkInput ix bytes =
    case digestFromByteString @Blake2b_256 bytes of
        Just txId -> Just $ TxInUtxo (AbstractHash txId) ix
        Nothing -> Nothing

-- | Construct a payment 'Output' for /Byron/ from primitive types.
--
-- __example__:
--
-- >>> mkOutput 42 =<< fromBase58 "Ae2tdPwU...DnXy319f"
-- Just (Output ...)
--
-- @since 1.0.0
mkOutput
    :: Natural
        -- ^ Output value, in Lovelace (1 Ada = 1e6 Lovelace).
    -> ByteString
        -- ^ Output Address. See also: 'fromBase58'.
    -> Maybe (Output Byron)
mkOutput n bytes =
    case (fromCBOR' bytes, mkLovelace (fromIntegral n)) of
        (Right addr, Right coin) -> Just $ TxOut addr coin
        _ -> Nothing
  where
    fromCBOR' = fmap snd . deserialiseFromBytes fromCBOR .  BL.fromStrict

-- | Construct a 'SignKey' for /Byron/ from primitive types.
--
-- __example__:
--
-- >>> mkSignKey =<< fromBase16 "3b402651...aad1c0b7"
-- Just (SignKey ...)
--
-- @since 1.0.0
mkSignKey
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
        -- See also: 'fromBase16'.
    -> Maybe (SignKey Byron)
mkSignKey bytes
    | BS.length bytes /= 96 = Nothing
    | otherwise = do
        let (prv, cc) = BS.splitAt 64 bytes
        pub <- ed25519ScalarMult (BS.take 32 prv)
        fmap SigningKey $ eitherToMaybe $ xprv $ prv <> pub <> cc
  where
    ed25519ScalarMult :: ByteString -> Maybe ByteString
    ed25519ScalarMult bs = do
        scalar <- eitherToMaybe $ eitherCryptoError $ Ed25519.scalarDecodeLong bs
        pure $ Ed25519.pointEncode $ Ed25519.toPoint scalar

--
-- ByteString Decoding
--

-- | Convert a base16 encoded 'Text' into a raw 'ByteString'
--
-- @since 1.0.0
fromBase16 :: Text -> Maybe ByteString
fromBase16 = eitherToMaybe . convertFromBase Base16 . T.encodeUtf8

-- | Convert a base58 encoded 'Text' into a raw 'ByteString'
--
-- @since 1.0.0
fromBase58 :: Text -> Maybe ByteString
fromBase58 = decodeBase58 bitcoinAlphabet . T.encodeUtf8


-- | Convert a base64 encoded 'Text' into a raw 'ByteString'
--
-- @since 1.0.0
fromBase64 :: Text -> Maybe ByteString
fromBase64 = eitherToMaybe . convertFromBase Base64 . T.encodeUtf8

--
-- MkPayment instance
--

-- Type-level constructor capturing types for 'Byron'.
data Byron

instance MkPayment Byron where
    type Init Byron = ProtocolMagicId

    type Input   Byron = TxIn
    type Output  Byron = TxOut
    type SignKey Byron = SigningKey

    type CoinSel Byron =
        (ProtocolMagicId, [TxIn], [TxOut])

    type Tx Byron = Either
        ErrMkPayment
        (ProtocolMagicId, NonEmpty TxIn, NonEmpty TxOut, TxSigData, [TxInWitness])

    empty :: ProtocolMagicId -> CoinSel Byron
    empty pm = (pm, mempty, mempty)

    addInput :: TxIn -> CoinSel Byron -> CoinSel Byron
    addInput inp (pm, inps, outs) = (pm, inp : inps, outs)

    addOutput :: TxOut -> CoinSel Byron -> CoinSel Byron
    addOutput out (pm, inps, outs) = (pm, inps, out : outs)

    lock :: CoinSel Byron -> Tx Byron
    lock (_pm, [], _outs) = Left MissingInput
    lock (_pm, _inps, []) = Left MissingOutput
    lock (pm, inps, outs) =
        Right (pm, neInps, neOuts, sigData, mempty)
      where
        sigData = TxSigData $ hash $ CC.UnsafeTx neInps neOuts (mkAttributes ())
        neInps  = NE.fromList $ reverse inps
        neOuts  = NE.fromList $ reverse outs

    signWith :: SigningKey -> Tx Byron -> Tx Byron
    signWith _ (Left e) = Left e
    signWith (SigningKey prv) (Right (pm, inps, outs, sigData, wits)) =
        Right (pm, inps, outs, sigData, VKWitness vk sig : wits)
      where
        vk :: VerificationKey
        vk = VerificationKey (toXPub prv)

        sig :: Signature TxSigData
        sig = CC.sign pm SignTx (SigningKey prv) sigData

    serialize :: Tx Byron -> Either ErrMkPayment ByteString
    serialize (Left e) = Left e
    serialize (Right (_pm, inps, outs, _sigData, wits))
        | NE.length inps /= length wits = Left MissingSignature
        | otherwise = Right $ CBOR.toStrictByteString $ toCBOR $ mkTxAux
            (CC.UnsafeTx inps outs (mkAttributes ()))
            (fromList $ reverse wits)


-- Internal
--
-- For running the Payment DSL as a command-line, we need to be able to produce
-- a text output representing the internal state.
--
-- There's no point about obfuscating this more than necessary; this should
-- remain mainly invisible to users if used properly through pipes. Yet, it can
-- be useful to have a human-friendly representation that is base16 or base64
-- encoded. Three possible obvious choice:
--
-- - CBOR, since most data above can already be serialized to CBOR
-- - JSON, instances exists on most type and are derived generically from CBOR
-- - Show, although here most types don't have a corresponding 'Read' instance.
--
-- Since we also need to decode an encoded state, CBOR will be path of least
-- resistance.

-- __Internal__: Encode a 'CoinSel Byron' to CBOR.
encodeCoinSel :: CoinSel Byron -> CBOR.Encoding
encodeCoinSel (pm, inps, outs) = mconcat
    [ toCBOR pm
    , CBOR.encodeListLenIndef
    , mconcat (toCBOR <$> inps)
    , CBOR.encodeBreak
    , CBOR.encodeListLenIndef
    , mconcat (toCBOR <$> outs)
    , CBOR.encodeBreak
    ]

-- __Internal__: Decode a 'CoinSel Byron' from CBOR.
decodeCoinSel :: CBOR.Decoder s (CoinSel Byron)
decodeCoinSel = (,,)
    <$> fromCBOR
    <*> decodeListIndef fromCBOR
    <*> decodeListIndef fromCBOR

-- __Internal__: Encode a 'Tx Byron' to CBOR.
encodeTx :: Tx Byron -> CBOR.Encoding
encodeTx (Left e) = mconcat
    [ CBOR.encodeWord8 0
    , CBOR.encodeString $ T.pack $ show e
    ]
encodeTx (Right (pm, inps, outs, sigData, wits)) = mconcat
    [ CBOR.encodeWord8 1
    , encodeCoinSel (pm, NE.toList inps, NE.toList outs)
    , toCBOR sigData
    , CBOR.encodeListLenIndef
    , mconcat (toCBOR <$> wits)
    , CBOR.encodeBreak
    ]

-- __Internal__: Decode a 'Tx Byron' from CBOR.
decodeTx :: CBOR.Decoder s (Tx Byron)
decodeTx = do
    CBOR.decodeWord8 >>= \case
        0 -> fmap T.unpack CBOR.decodeString >>= \case
            str | str == show MissingInput     -> pure $ Left MissingInput
            str | str == show MissingOutput    -> pure $ Left MissingOutput
            str | str == show MissingSignature -> pure $ Left MissingSignature
            _ -> fail $
                "Invalid error constructor found in 'Tx Byron'. Constructor must \
                \be one of: " <> unwords (show <$>
                    [ MissingInput, MissingOutput, MissingSignature])
        1 -> do
            (pm, inps, outs) <- decodeCoinSel
            sigData <- fromCBOR
            wits <- decodeListIndef fromCBOR
            case (nonEmpty inps, nonEmpty outs) of
                (Nothing, _) -> fail
                    "Empty list of inputs found in 'Tx Byron'. This is impossible \
                    \unless the data has been modified by hand."
                (_, Nothing) -> fail
                    "Empty list of outputs found in 'Tx Byron'. This is impossible \
                    \unless the data has been modified by hand."
                (Just neInps, Just neOuts) ->
                    pure $ Right (pm, neInps, neOuts, sigData, wits)

        _ -> fail
            "'Tx Byron' has been modified  with and is now invalid. The first \
            \byte must be either 0 or 1, followed by respectively an error \
            \constructor or constituants of the intermediate transaction."

-- __Internal__ Decode an arbitrary long list 'sandwiched' by markers.
decodeListIndef :: forall s a. CBOR.Decoder s a -> CBOR.Decoder s [a]
decodeListIndef decodeOne = do
    _ <- CBOR.decodeListLenIndef
    CBOR.decodeSequenceLenIndef (flip (:)) [] reverse decodeOne
