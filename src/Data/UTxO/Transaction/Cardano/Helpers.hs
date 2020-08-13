{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_HADDOCK prune #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-} -- needed for {#- HLINT ... #-}

module Data.UTxO.Transaction.Cardano.Helpers
    (
    -- * Converting From Bases
      fromBase16
    , fromBase58
    , fromBase64
    , fromBech32

    , toBase16

    , ed25519ScalarMult
    ) where

import Crypto.Error
    ( eitherCryptoError )
import Data.ByteArray.Encoding
    ( Base (..), convertFromBase, convertToBase )
import Data.ByteString
    ( ByteString )
import Data.ByteString.Base58
    ( bitcoinAlphabet, decodeBase58 )
import Data.Either.Extra
    ( eitherToMaybe )
import Data.Text
    ( Text )

import qualified Codec.Binary.Bech32 as Bech32
import qualified Crypto.ECC.Edwards25519 as Ed25519
import qualified Data.Text.Encoding as T

--
-- ByteString Decoding
--

-- | Convert a base16 encoded 'Text' into a raw 'ByteString'
--
-- @since 1.0.0
fromBase16 :: Text -> Maybe ByteString
fromBase16 = eitherToMaybe . convertFromBase Base16 . T.encodeUtf8

-- | Convert a a raw 'ByteString' into base16 encoded 'Text'
--
-- @since 2.0.0
toBase16 :: ByteString -> Text
toBase16 = T.decodeUtf8 . convertToBase Base16


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

-- | Convert a Bech32 encoded 'Text' into a raw 'ByteString'
--
-- @since 2.0.0
fromBech32 :: Text -> Maybe ByteString
fromBech32 txt = do
    (_, dp) <- either (const Nothing) Just (Bech32.decodeLenient txt)
    Bech32.dataPartToBytes dp

ed25519ScalarMult :: ByteString -> Maybe ByteString
ed25519ScalarMult bs = do
    scalar <- eitherToMaybe $ eitherCryptoError $ Ed25519.scalarDecodeLong bs
    pure $ Ed25519.pointEncode $ Ed25519.toPoint scalar
