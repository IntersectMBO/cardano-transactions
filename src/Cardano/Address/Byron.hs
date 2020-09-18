{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_HADDOCK hide #-}

module Cardano.Address.Byron
    ( Address (..)
    , AddrAttributes (..)
    , getAddrAttributes
    , mkAttributes
    ) where

import Cardano.Chain.Common
    ( AddrAttributes (..)
    , HDAddressPayload (..)
    , NetworkMagic (..)
    , mkAttributes
    )
import Control.Monad
    ( replicateM )
import Control.Monad.Fail
    ( MonadFail )
import Data.ByteString
    ( ByteString )
import Data.Either.Extra
    ( eitherToMaybe )
import Data.Word
    ( Word8 )
import GHC.Stack
    ( HasCallStack )

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Data.ByteString.Lazy as BL


-- | A type isomorphic to an 'ByteString' to represent addresses.
newtype Address = Address
    { unAddress :: ByteString }
    deriving (Show)

-- | Re-construct address attributes from an address
getAddrAttributes :: Address -> Maybe AddrAttributes
getAddrAttributes addr = do
    attributes <- deserializeAttributes addr
    let networkMagic = findNetworkMagic attributes
    let hdPayload = findHDPayload attributes
    pure $ AddrAttributes hdPayload networkMagic

-- | Extract an HD payload from a list of raw address attributes.
findHDPayload :: [(Word8, ByteString)] -> Maybe HDAddressPayload
findHDPayload attributes =
    case filter (\(tag,_) -> tag == 1) attributes of
        [(1, bytes)] ->
            HDAddressPayload <$> decodeNestedBytes CBOR.decodeBytes bytes
        _ ->
            Nothing

-- | Extract the network magic from a list of raw address attributes.
--
-- NOTE: the attribute is only present for 'testnet' addresses, so the absence
-- of attributes indicates a 'Mainnet' or 'Staging' address.
findNetworkMagic :: HasCallStack => [(Word8, ByteString)] -> NetworkMagic
findNetworkMagic attributes =
    case filter (\(tag, _) -> tag == 2) attributes of
        [(2, bytes)] ->
            case deserialiseCbor CBOR.decodeInt32 bytes of
                Nothing -> error "malformed network magic in address?"
                Just pm -> NetworkTestnet $ fromIntegral pm
        _ ->
            NetworkMainOrStage

-- | Extract the HD Payload from a set of raw attributes
deserializeAttributes :: Address -> Maybe [(Word8, ByteString)]
deserializeAttributes (Address addr) = do
    payload <- deserialiseCbor decodeAddressPayload addr
    deserialiseCbor decodeAllAttributes' payload
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

--
-- CBOR helpers
--

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
