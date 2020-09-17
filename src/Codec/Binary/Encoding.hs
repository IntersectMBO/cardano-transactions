{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_HADDOCK prune #-}

module Codec.Binary.Encoding
    (
    -- * Converting From Bases
      fromBase16
    , fromBech32
    , fromBase58
    , fromBase64

    -- * Conversion To Bases
    , base16
    , bech32
    , base58
    , base64
    ) where

import Codec.Binary.Bech32
    ( HumanReadablePart )
import Data.ByteArray.Encoding
    ( Base (..), convertFromBase, convertToBase )
import Data.ByteString
    ( ByteString )
import Data.ByteString.Base58
    ( bitcoinAlphabet, decodeBase58, encodeBase58 )
import Data.Either.Extra
    ( eitherToMaybe )
import Data.Text
    ( Text )

import qualified Codec.Binary.Bech32 as Bech32
import qualified Data.Text.Encoding as T

-- | Convert a base16 encoded 'Text' into a raw 'ByteString'
--
-- @since 2.0.0
fromBase16 :: Text -> Maybe ByteString
fromBase16 = eitherToMaybe . convertFromBase Base16 . T.encodeUtf8

-- | Convert a raw 'ByteString' into base16 encoded 'Text'
--
-- @since 2.0.0
base16 :: ByteString -> Text
base16 = T.decodeUtf8 . convertToBase Base16

-- | Convert a Bech32 encoded 'Text' into a raw 'ByteString'
--
-- @since 2.0.0
fromBech32 :: Text -> Maybe ByteString
fromBech32 txt = do
    (_, dp) <- either (const Nothing) Just (Bech32.decodeLenient txt)
    Bech32.dataPartToBytes dp

-- | Convert a raw 'ByteString' into a bech32 encoded 'Text'
--
-- @since 2.0.0
bech32 :: HumanReadablePart -> ByteString -> Text
bech32 hrp bytes =
    Bech32.encodeLenient hrp (Bech32.dataPartFromBytes bytes)

-- | Convert a base58 encoded 'Text' into a raw 'ByteString'
--
-- @since 2.0.0
fromBase58 :: Text -> Maybe ByteString
fromBase58 = decodeBase58 bitcoinAlphabet . T.encodeUtf8

-- | Convert a raw 'ByteString' into a base58 encoded 'Text'
base58 :: ByteString -> Text
base58 = T.decodeUtf8 . encodeBase58 bitcoinAlphabet

-- | Convert a base64 encoded 'Text' into a raw 'ByteString'
--
-- @since 2.0.0
fromBase64 :: Text -> Maybe ByteString
fromBase64 = eitherToMaybe . convertFromBase Base64 . T.encodeUtf8

-- | Convert a raw 'ByteString' into a base64 encoded 'Text', with padding.
--
-- @since 2.0.0
base64 :: ByteString -> Text
base64 = T.decodeUtf8 . convertToBase Base64
