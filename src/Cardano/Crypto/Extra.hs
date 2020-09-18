{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_HADDOCK hide #-}

module Cardano.Crypto.Extra
    ( xprvFromBytes
    ) where

import Cardano.Crypto.Wallet
    ( XPrv, xprv )
import Crypto.Error
    ( eitherCryptoError )
import Data.ByteString
    ( ByteString )
import Data.Either.Extra
    ( eitherToMaybe )

import qualified Crypto.ECC.Edwards25519 as Ed25519
import qualified Data.ByteString as BS

-- | Convert a raw 96-byte 'ByteString' into a @cardano-crypto@ 'XPrv'
--
-- In memory, XPrv are basically a concatenation of three parts:
--
--     (encrypted) private key | public key | chain code
--     <----------------------> <----------> <---------->
--             64 bytes           32 bytes     32 bytes
--
-- NOTE 1:
--   The input bytestring is expected to be made of only the encrypted private
--   part and the chain code, while the public part is re-calculated from the
--   private part.
--
-- NOTE 2:
--   This module, we do not encrypt the private part in-memory, but it could be
--   in practice.
xprvFromBytes :: ByteString -> Maybe XPrv
xprvFromBytes bytes
    | BS.length bytes /= 96 = Nothing
    | otherwise = do
        let (prv, cc) = BS.splitAt 64 bytes
        pub <- ed25519ScalarMult (BS.take 32 prv)
        eitherToMaybe $ xprv $ prv <> pub <> cc
  where
      ed25519ScalarMult :: ByteString -> Maybe ByteString
      ed25519ScalarMult bs = do
          scalar <- eitherToMaybe $ eitherCryptoError $ Ed25519.scalarDecodeLong bs
          pure $ Ed25519.pointEncode $ Ed25519.toPoint scalar
