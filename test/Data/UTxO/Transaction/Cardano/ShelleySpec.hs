{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.UTxO.Transaction.Cardano.ShelleySpec
    ( spec
    ) where

import Prelude

import Codec.Binary.Encoding
    ( base16, fromBase16, fromBase58, fromBech32 )
import Control.Monad
    ( (>=>) )
import Data.ByteString
    ( ByteString )
import Data.Function
    ( (&) )
import Data.Maybe
    ( fromJust, fromMaybe, isNothing )
import Data.Text
    ( Text )
import Data.UTxO.Transaction
    ( ErrMkPayment (..), MkPayment (..) )
import Data.UTxO.Transaction.Cardano.Shelley
    ( NetworkId (..)
    , NetworkMagic (..)
    , Shelley
    , mkAddrAttributes
    , mkByronSignKey
    , mkInit
    , mkInput
    , mkOutput
    , mkShelleySignKey
    )
import Data.Word
    ( Word32, Word64, Word8 )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( Spec, describe, expectationFailure, it, shouldBe )
import Test.Hspec.QuickCheck
    ( prop )
import Test.QuickCheck
    ( Arbitrary (..)
    , conjoin
    , counterexample
    , forAll
    , isSuccess
    , oneof
    , quickCheckResult
    , suchThat
    , withMaxSuccess
    , (===)
    )

import qualified Cardano.Api.Typed as Api
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Pretty as CBOR
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.UTxO.Transaction as Tx
{-# ANN spec ("HLint: ignore Use head" :: String) #-}

spec :: Spec
spec = do
    describe "Negative tests" $ do
        prop "Invalid Input" $ \ix -> do
            let len = 32
            let genInvalidId = oneof
                    [ pure ""
                    , pure $ BS.replicate (len - 1) 0
                    , pure $ BS.replicate (len + 1) 0
                    , (BS.pack <$> arbitrary) `suchThat` ((/= len) . BS.length)
                    ]
            forAll genInvalidId (isNothing . mkInput ix)

        prop "Invalid Output" $ \(coin :: Word64) -> do
            let genInvalidAddress =
                    (BS.pack <$> arbitrary) `suchThat` ((< 30) . BS.length)
            forAll genInvalidAddress (isNothing . mkOutput (fromIntegral coin))

        prop "Invalid SignKey" $ do
            let len = 96
            let genInvalidSignKey =
                    (BS.pack <$> arbitrary) `suchThat` ((/= len) . BS.length)
            forAll genInvalidSignKey (isNothing . mkShelleySignKey)

        it "Missing Input" $ do
            let result = Tx.empty dummyInit
                       & Tx.addOutput (unsafeMkOutput 14 addrTxt)
                       & Tx.lock
                       & Tx.serialize
            result `shouldBe` Left MissingInput

        it "Missing Output" $ do
            let result = Tx.empty dummyInit
                       & Tx.addInput (unsafeMkInput 0 txidTxt)
                       & Tx.lock
                       & Tx.serialize
            result `shouldBe` Left MissingOutput

        it "Missing Signature" $ do
            let result = Tx.empty dummyInit
                       & Tx.addInput (unsafeMkInput 0 txidTxt)
                       & Tx.addOutput (unsafeMkOutput 1400000 addrTxt)
                       & Tx.lock
                       & Tx.serialize
            result `shouldBe` Left MissingSignature

    describe "Golden tests" $ do
        it "1 input, 2 outputs (DSL) - Shelley witnesses" $ do
            let amtInp = 10000000
            let amtFee = 129700
            let amtOut = 2000000
            let amtChange = amtInp - amtOut - amtFee
            compareGolden integrationTesting__1_2_shelley $
                Tx.empty (mkInit Mainnet 7750 (fromIntegral amtFee))
                & Tx.addInput (unsafeMkInputFromBS 0 txid)
                & Tx.addOutput (unsafeMkOutputFromBS amtOut (addr 1))
                & Tx.addOutput (unsafeMkOutputFromBS amtChange (addr 2))
                & Tx.lock
                & Tx.signWith (unsafeMkShelleySignKeyFromBS (wit 0))
                & Tx.serialize

        it "2 inputs, 3 outputs (DSL) - Shelley witnesses" $ do
            let amtInp1 = 10000000
            let amtInp2 = 10000000
            let amtFee = 135200
            let amtOut1 = 6000000
            let amtOut2 = 6000000
            let amtChange = amtInp1 + amtInp2 - amtOut1 - amtOut2 - amtFee
            compareGolden integrationTesting__2_3_shelley $
                Tx.empty (mkInit Mainnet 7750 (fromIntegral amtFee))
                & Tx.addInput (unsafeMkInputFromBS 0 txid)
                & Tx.addInput (unsafeMkInputFromBS 1 txid)
                & Tx.addOutput (unsafeMkOutputFromBS amtOut1 (addr 2))
                & Tx.addOutput (unsafeMkOutputFromBS amtOut2 (addr 3))
                & Tx.addOutput (unsafeMkOutputFromBS amtChange (addr 4))
                & Tx.lock
                & Tx.signWith (unsafeMkShelleySignKeyFromBS (wit 0))
                & Tx.signWith (unsafeMkShelleySignKeyFromBS (wit 1))
                & Tx.serialize

        it "1 input, 2 outputs (DSL) - Byron witnesses - mainnet" $ do
            let amtInp = 10000000
            let amtFee = 129700
            let amtOut = 2000000
            let amtChange = amtInp - amtOut - amtFee
            compareGolden integrationTesting__1_2_byron_mainnet $
                Tx.empty (mkInit Mainnet 7750 (fromIntegral amtFee))
                & Tx.addInput (unsafeMkInputFromBS 0 txid)
                & Tx.addOutput (unsafeMkOutputFromBS amtOut (addr 1))
                & Tx.addOutput (unsafeMkOutputFromBS amtChange (addr 2))
                & Tx.lock
                & Tx.signWith (unsafeMkByronSignKeyFromBS (byronAddrs !! 0) (wit 0))
                & Tx.serialize

        it "1 input, 2 outputs (DSL) - Byron witnesses - testnet" $ do
            let amtInp = 10000000
            let amtFee = 129700
            let amtOut = 2000000
            let amtChange = amtInp - amtOut - amtFee
            compareGolden integrationTesting__1_2_byron_testnet $
                Tx.empty (mkInit (Testnet (NetworkMagic 0)) 7750 (fromIntegral amtFee))
                & Tx.addInput (unsafeMkInputFromBS 0 txid)
                & Tx.addOutput (unsafeMkOutputFromBS amtOut (addr 1))
                & Tx.addOutput (unsafeMkOutputFromBS amtChange (addr 2))
                & Tx.lock
                & Tx.signWith (unsafeMkByronSignKeyFromBS (byronAddrs !! 2) (wit 0))
                & Tx.serialize

        it "2 inputs, 3 outputs (DSL) - Byron witnesses - mainnet" $ do
            let amtInp1 = 10000000
            let amtInp2 = 10000000
            let amtFee = 135200
            let amtOut1 = 6000000
            let amtOut2 = 6000000
            let amtChange = amtInp1 + amtInp2 - amtOut1 - amtOut2 - amtFee
            compareGolden integrationTesting__2_3_byron_mainnet $
                Tx.empty (mkInit Mainnet 7750 (fromIntegral amtFee))
                & Tx.addInput (unsafeMkInputFromBS 0 txid)
                & Tx.addInput (unsafeMkInputFromBS 1 txid)
                & Tx.addOutput (unsafeMkOutputFromBS amtOut1 (addr 2))
                & Tx.addOutput (unsafeMkOutputFromBS amtOut2 (addr 3))
                & Tx.addOutput (unsafeMkOutputFromBS amtChange (addr 4))
                & Tx.lock
                & Tx.signWith (unsafeMkByronSignKeyFromBS (byronAddrs !! 0) (wit 0))
                & Tx.signWith (unsafeMkByronSignKeyFromBS (byronAddrs !! 1) (wit 1))
                & Tx.serialize

        it "2 inputs, 3 outputs (DSL) - Byron witnesses - testnet" $ do
            let amtInp1 = 10000000
            let amtInp2 = 10000000
            let amtFee = 135200
            let amtOut1 = 6000000
            let amtOut2 = 6000000
            let amtChange = amtInp1 + amtInp2 - amtOut1 - amtOut2 - amtFee
            compareGolden integrationTesting__2_3_byron_testnet $
                Tx.empty (mkInit (Testnet (NetworkMagic 0)) 7750 (fromIntegral amtFee))
                & Tx.addInput (unsafeMkInputFromBS 0 txid)
                & Tx.addInput (unsafeMkInputFromBS 1 txid)
                & Tx.addOutput (unsafeMkOutputFromBS amtOut1 (addr 2))
                & Tx.addOutput (unsafeMkOutputFromBS amtOut2 (addr 3))
                & Tx.addOutput (unsafeMkOutputFromBS amtChange (addr 4))
                & Tx.lock
                & Tx.signWith (unsafeMkByronSignKeyFromBS (byronAddrs !! 2) (wit 0))
                & Tx.signWith (unsafeMkByronSignKeyFromBS (byronAddrs !! 3) (wit 1))
                & Tx.serialize

    describe "Golden tests for multisig scripts" $ do
        let verKey1 = Api.RequireSignature
                $ convertToHash "deeae4e895d8d57378125ed4fd540f9bf245d59f7936a504379cfc1e"
        let verKey2 = Api.RequireSignature
                $ convertToHash "60a3bf69aa748f9934b64357d9f1ca202f1a768aaf57263aedca8d5f"
        let verKey3 = Api.RequireSignature
                $ convertToHash "ffcbb72393215007d9a0aa02b7430080409cd8c053fd4f5b4d905053"
        let verKey4 = Api.RequireSignature
                $ convertToHash "96834025cdca063ce9c32dfae6bc6a3e47f8da07ee4fb8e1a3901559"

        it "RequireSignature index=0" $ do
            let script = Api.makeMultiSigScript verKey1
            base16 (Api.serialiseToCBOR script) `shouldBe`
                "82008200581cdeeae4e895d8d57378125ed4fd540f9bf245d59f7936a504379cfc1e"
            base16 (Api.serialiseToRawBytes $ Api.scriptHash script) `shouldBe`
                "59fd497a34ac3e5abf2c8a703e3aaf3a2750e207b139d65d08d2c1b3"

        it "RequireSignature index=1" $ do
            let script = Api.makeMultiSigScript verKey2
            base16 (Api.serialiseToCBOR script) `shouldBe`
                "82008200581c60a3bf69aa748f9934b64357d9f1ca202f1a768aaf57263aedca8d5f"
            base16 (Api.serialiseToRawBytes $ Api.scriptHash script) `shouldBe`
                "8d60cfd18163231751491389db7fa95bbb4192452d493f4147949f42"

        it "RequireSignature index=2" $ do
            let script = Api.makeMultiSigScript verKey3
            base16 (Api.serialiseToCBOR script) `shouldBe`
                "82008200581cffcbb72393215007d9a0aa02b7430080409cd8c053fd4f5b4d905053"
            base16 (Api.serialiseToRawBytes $ Api.scriptHash script) `shouldBe`
                "f34b7c6642ee2d2ff115a09d60d084d8df866c1be7722bfb78585d75"

        it "RequireSignature index=3" $ do
            let script = Api.makeMultiSigScript verKey4
            base16 (Api.serialiseToCBOR script) `shouldBe`
                "82008200581c96834025cdca063ce9c32dfae6bc6a3e47f8da07ee4fb8e1a3901559"
            base16 (Api.serialiseToRawBytes $ Api.scriptHash script) `shouldBe`
                "324f9577142f2034545ba2e905d6e8a18afbd3ede08ec6c274aa641b"

        it "RequireAllOf - two ver keys" $ do
            let multisig = Api.RequireAllOf [verKey1, verKey2]
            let script = Api.makeMultiSigScript multisig
            base16 (Api.serialiseToCBOR script) `shouldBe`
                "82008201828200581cdeeae4e895d8d57378125ed4fd540f9bf245d59f7936a50437\
                \9cfc1e8200581c60a3bf69aa748f9934b64357d9f1ca202f1a768aaf57263aedca8d5f"
            base16 (Api.serialiseToRawBytes $ Api.scriptHash script) `shouldBe`
                "57eec18300169c459169e335b809dbf50ca236d6ef730f623799a004"

        it "RequireAllOf - three ver keys" $ do
            let multisig = Api.RequireAllOf [verKey1, verKey2, verKey3]
            let script = Api.makeMultiSigScript multisig
            base16 (Api.serialiseToCBOR script) `shouldBe`
                "82008201838200581cdeeae4e895d8d57378125ed4fd540f9bf245d59f7936a504379cfc1e8200\
                \581c60a3bf69aa748f9934b64357d9f1ca202f1a768aaf57263aedca8d5f8200581cffcbb72393\
                \215007d9a0aa02b7430080409cd8c053fd4f5b4d905053"
            base16 (Api.serialiseToRawBytes $ Api.scriptHash script) `shouldBe`
                "3db6ba0c234043ab963f5cc723d8a953d46477dbdc45f3dbd73847f1"

        it "RequireAnyOf - two ver keys" $ do
            let multisig = Api.RequireAnyOf [verKey1, verKey2]
            let script = Api.makeMultiSigScript multisig
            base16 (Api.serialiseToCBOR script) `shouldBe`
                "82008202828200581cdeeae4e895d8d57378125ed4fd540f9bf245d59f7936a50437\
                \9cfc1e8200581c60a3bf69aa748f9934b64357d9f1ca202f1a768aaf57263aedca8d5f"
            base16 (Api.serialiseToRawBytes $ Api.scriptHash script) `shouldBe`
                "ee98a536acf306c398d0b51a675a088faf76d7e41d1f0ab94efc3be7"

        it "RequireAnyOf - three ver keys" $ do
            let multisig = Api.RequireAnyOf [verKey1, verKey2, verKey3]
            let script = Api.makeMultiSigScript multisig
            base16 (Api.serialiseToCBOR script) `shouldBe`
                "82008202838200581cdeeae4e895d8d57378125ed4fd540f9bf245d59f7936a504379cfc1e8200\
                \581c60a3bf69aa748f9934b64357d9f1ca202f1a768aaf57263aedca8d5f8200581cffcbb72393\
                \215007d9a0aa02b7430080409cd8c053fd4f5b4d905053"
            base16 (Api.serialiseToRawBytes $ Api.scriptHash script) `shouldBe`
                "270cbddf1d43fb4ad7eca05f08f2c9c65a290389d8c48c57ba9f38c4"

        it "RequireMOf - 1 out of two ver keys" $ do
            let multisig = Api.RequireMOf 1 [verKey1, verKey2]
            let script = Api.makeMultiSigScript multisig
            base16 (Api.serialiseToCBOR script) `shouldBe`
                "8200830301828200581cdeeae4e895d8d57378125ed4fd540f9bf245d59f7936a504\
                \379cfc1e8200581c60a3bf69aa748f9934b64357d9f1ca202f1a768aaf57263aedca8d5f"
            base16 (Api.serialiseToRawBytes $ Api.scriptHash script) `shouldBe`
                "31aa5030ae386603145f0cb16577da64ce0647b3cf2104e8d5646d67"

        it "RequireMOf - 2 out of four ver keys" $ do
            let multisig = Api.RequireMOf 2 [verKey1, verKey2, verKey3, verKey4]
            let script = Api.makeMultiSigScript multisig
            base16 (Api.serialiseToCBOR script) `shouldBe`
                "8200830302848200581cdeeae4e895d8d57378125ed4fd540f9bf245d59f7936a504379cfc1e82\
                \00581c60a3bf69aa748f9934b64357d9f1ca202f1a768aaf57263aedca8d5f8200581cffcbb723\
                \93215007d9a0aa02b7430080409cd8c053fd4f5b4d9050538200581c96834025cdca063ce9c32d\
                \fae6bc6a3e47f8da07ee4fb8e1a3901559"
            base16 (Api.serialiseToRawBytes $ Api.scriptHash script) `shouldBe`
                "e2da1830b3465ae1a9161e89ff79673d8d133c841ab06418f0034534"

        it "nested - 1" $ do
            let nested = Api.RequireAllOf [verKey3, verKey4]
            let multisig = Api.RequireMOf 2 [verKey1, verKey2, nested]
            let script = Api.makeMultiSigScript multisig
            base16 (Api.serialiseToCBOR script) `shouldBe`
                "8200830302838200581cdeeae4e895d8d57378125ed4fd540f9bf245d59f7936a504379cfc1e8\
                \200581c60a3bf69aa748f9934b64357d9f1ca202f1a768aaf57263aedca8d5f8201828200581c\
                \ffcbb72393215007d9a0aa02b7430080409cd8c053fd4f5b4d9050538200581c96834025cdca0\
                \63ce9c32dfae6bc6a3e47f8da07ee4fb8e1a3901559"
            base16 (Api.serialiseToRawBytes $ Api.scriptHash script) `shouldBe`
                "1ae2515f480c8b67fc2fcccab565bbc12b196a24083f6cf278c3ed7a"

        it "nested - 2" $ do
            let nested = Api.RequireAnyOf [verKey2, verKey3, verKey4]
            let multisig = Api.RequireAllOf [verKey1, nested]
            let script = Api.makeMultiSigScript multisig
            base16 (Api.serialiseToCBOR script) `shouldBe`
                "82008201828200581cdeeae4e895d8d57378125ed4fd540f9bf245d59f7936a504379cfc1e82\
                \02838200581c60a3bf69aa748f9934b64357d9f1ca202f1a768aaf57263aedca8d5f8200581c\
                \ffcbb72393215007d9a0aa02b7430080409cd8c053fd4f5b4d9050538200581c96834025cdca\
                \063ce9c32dfae6bc6a3e47f8da07ee4fb8e1a3901559"
            base16 (Api.serialiseToRawBytes $ Api.scriptHash script) `shouldBe`
                "ae743bad455bad082ab69fd31183fb70a2787274938c85048b80e8ee"

        it "nested - 3" $ do
            let nestedInner = Api.RequireAnyOf [verKey3, verKey4]
            let nested = Api.RequireAllOf [verKey1, nestedInner]
            let multisig = Api.RequireMOf 1 [verKey1, nested]
            let script = Api.makeMultiSigScript multisig
            base16 (Api.serialiseToCBOR script) `shouldBe`
                "8200830301828200581cdeeae4e895d8d57378125ed4fd540f9bf245d59f7936a504379cfc1e8\
                \201828200581cdeeae4e895d8d57378125ed4fd540f9bf245d59f7936a504379cfc1e82028282\
                \00581cffcbb72393215007d9a0aa02b7430080409cd8c053fd4f5b4d9050538200581c9683402\
                \5cdca063ce9c32dfae6bc6a3e47f8da07ee4fb8e1a3901559"
            base16 (Api.serialiseToRawBytes $ Api.scriptHash script) `shouldBe`
                "8aa7af44362310140ff3d32ac7d1a2ecbe26da65f3d146c64b90e9e1"

convertToHash :: Text -> Api.Hash Api.PaymentKey
convertToHash txt =
  case Api.deserialiseFromRawBytesHex (Api.AsHash Api.AsPaymentKey) $ T.encodeUtf8 txt of
    Just payKeyHash -> payKeyHash
    Nothing -> error $ "Test.Cardano.Api.Examples.convertToHash: Error deserialising payment key hash: "
               <> T.unpack txt

--
-- Golden
--

compareGolden
    :: Show e
    => ByteString
        -- ^ An expected encoded result
    -> Either e ByteString
        -- ^ The actual string received
    -> IO ()
compareGolden _want (Left e)   = expectationFailure (show e)
compareGolden want (Right got) = quickCheck' $ withMaxSuccess 1 $
    -- NOTE Using QuickCheck here simply for getting better counter examples
    -- than HSpec in case of failure...
    conjoin (uncurry (===) <$> zip (lines prettyWant) (lines prettyGot))
        & counterexample ("Got:  " <> prettyGot)
        & counterexample ("Want: " <> prettyWant)
  where
    prettyWant =
        CBOR.prettyHexEnc $ CBOR.encodePreEncoded want
    prettyGot  =
        CBOR.prettyHexEnc $ CBOR.encodePreEncoded got
    quickCheck' = quickCheckResult >=> failOnFailure
    failOnFailure result
        | isSuccess result = pure ()
        | otherwise        = expectationFailure (show result)

integrationTesting__1_2_shelley :: ByteString
integrationTesting__1_2_shelley = unsafeB16
    "83a400818258200000000000000000000000000000000000000000000000000000\
    \000000000000000182825839010101010101010101010101010101010101010101\
    \010101010101010101010101010101010101010101010101010101010101010101\
    \0101011a001e848082583901020202020202020202020202020202020202020202\
    \020202020202020202020202020202020202020202020202020202020202020202\
    \02021a0078175c021a0001faa403191e46a1008182582001000000000000000000\
    \000000000000000000000000000000000000000000005840d7af60ae33d2af3514\
    \11c1445c79590526990bfa73cbb3732b54ef322daa142e6884023410f8be3c16e9\
    \bd52076f2bb36bf38dfe034a9f04658e9f56197ab80ff6"

integrationTesting__2_3_shelley :: ByteString
integrationTesting__2_3_shelley = unsafeB16
    "83a400828258200000000000000000000000000000000000000000000000000000\
    \000000000000008258200000000000000000000000000000000000000000000000\
    \000000000000000000010183825839010202020202020202020202020202020202\
    \020202020202020202020202020202020202020202020202020202020202020202\
    \0202020202021a005b8d8082583901030303030303030303030303030303030303\
    \030303030303030303030303030303030303030303030303030303030303030303\
    \03030303031a005b8d808258390104040404040404040404040404040404040404\
    \040404040404040404040404040404040404040404040404040404040404040404\
    \040404041a007801e0021a0002102003191e46a10082825820130ae82201d7072e\
    \6fbfc0a1884fb54636554d14945b799125cf7ce38d477f5158405835ff78c6fc5e\
    \4466a179ca659fa85c99b8a3fba083f3f3f42ba360d479c64ef169914b52ade49b\
    \19a7208fd63a6e67a19c406b4826608fdc5307025506c307825820010000000000\
    \00000000000000000000000000000000000000000000000000005840e8e769ecd0\
    \f3c538f0a5a574a1c881775f086d6f4c845b81be9b78955728bffa7efa54297c6a\
    \5d73337bd6280205b1759c13f79d4c93f29871fc51b78aeba80ef6"

integrationTesting__1_2_byron_mainnet :: ByteString
integrationTesting__1_2_byron_mainnet = unsafeB16
    "83a4008182582000000000000000000000000000000000000000000000000000\
    \0000000000000000018282583901010101010101010101010101010101010101\
    \0101010101010101010101010101010101010101010101010101010101010101\
    \0101010101011a001e8480825839010202020202020202020202020202020202\
    \0202020202020202020202020202020202020202020202020202020202020202\
    \020202020202021a0078175c021a0001faa403191e46a1028184582001000000\
    \000000000000000000000000000000000000000000000000000000005840d7af\
    \60ae33d2af351411c1445c79590526990bfa73cbb3732b54ef322daa142e6884\
    \023410f8be3c16e9bd52076f2bb36bf38dfe034a9f04658e9f56197ab80f5820\
    \0000000000000000000000000000000000000000000000000000000000000000\
    \41a0f6"

integrationTesting__1_2_byron_testnet :: ByteString
integrationTesting__1_2_byron_testnet = unsafeB16
    "83a4008182582000000000000000000000000000000000000000000000000000\
    \0000000000000000018282583901010101010101010101010101010101010101\
    \0101010101010101010101010101010101010101010101010101010101010101\
    \0101010101011a001e8480825839010202020202020202020202020202020202\
    \0202020202020202020202020202020202020202020202020202020202020202\
    \020202020202021a0078175c021a0001faa403191e46a1028184582001000000\
    \000000000000000000000000000000000000000000000000000000005840d7af\
    \60ae33d2af351411c1445c79590526990bfa73cbb3732b54ef322daa142e6884\
    \023410f8be3c16e9bd52076f2bb36bf38dfe034a9f04658e9f56197ab80f5820\
    \0000000000000000000000000000000000000000000000000000000000000000\
    \48a102451a4170cb17f6"

integrationTesting__2_3_byron_mainnet :: ByteString
integrationTesting__2_3_byron_mainnet = unsafeB16
    "83a4008282582000000000000000000000000000000000000000000000000000\
    \0000000000000000825820000000000000000000000000000000000000000000\
    \0000000000000000000000010183825839010202020202020202020202020202\
    \0202020202020202020202020202020202020202020202020202020202020202\
    \020202020202020202021a005b8d808258390103030303030303030303030303\
    \0303030303030303030303030303030303030303030303030303030303030303\
    \03030303030303030303031a005b8d8082583901040404040404040404040404\
    \0404040404040404040404040404040404040404040404040404040404040404\
    \0404040404040404040404041a007801e0021a0002102003191e46a102828458\
    \2001000000000000000000000000000000000000000000000000000000000000\
    \005840e8e769ecd0f3c538f0a5a574a1c881775f086d6f4c845b81be9b789557\
    \28bffa7efa54297c6a5d73337bd6280205b1759c13f79d4c93f29871fc51b78a\
    \eba80e5820000000000000000000000000000000000000000000000000000000\
    \000000000041a0845820130ae82201d7072e6fbfc0a1884fb54636554d14945b\
    \799125cf7ce38d477f5158405835ff78c6fc5e4466a179ca659fa85c99b8a3fb\
    \a083f3f3f42ba360d479c64ef169914b52ade49b19a7208fd63a6e67a19c406b\
    \4826608fdc5307025506c3075820010101010101010101010101010101010101\
    \010101010101010101010101010141a0f6"

integrationTesting__2_3_byron_testnet :: ByteString
integrationTesting__2_3_byron_testnet = unsafeB16
    "83a4008282582000000000000000000000000000000000000000000000000000\
    \0000000000000000825820000000000000000000000000000000000000000000\
    \0000000000000000000000010183825839010202020202020202020202020202\
    \0202020202020202020202020202020202020202020202020202020202020202\
    \020202020202020202021a005b8d808258390103030303030303030303030303\
    \0303030303030303030303030303030303030303030303030303030303030303\
    \03030303030303030303031a005b8d8082583901040404040404040404040404\
    \0404040404040404040404040404040404040404040404040404040404040404\
    \0404040404040404040404041a007801e0021a0002102003191e46a102828458\
    \2001000000000000000000000000000000000000000000000000000000000000\
    \005840e8e769ecd0f3c538f0a5a574a1c881775f086d6f4c845b81be9b789557\
    \28bffa7efa54297c6a5d73337bd6280205b1759c13f79d4c93f29871fc51b78a\
    \eba80e5820000000000000000000000000000000000000000000000000000000\
    \000000000048a102451a4170cb17845820130ae82201d7072e6fbfc0a1884fb5\
    \4636554d14945b799125cf7ce38d477f5158405835ff78c6fc5e4466a179ca65\
    \9fa85c99b8a3fba083f3f3f42ba360d479c64ef169914b52ade49b19a7208fd6\
    \3a6e67a19c406b4826608fdc5307025506c30758200101010101010101010101\
    \01010101010101010101010101010101010101010148a102451a4170cb17f6"

--
-- Internal
--
dummyInit :: Init Shelley
dummyInit = mkInit Mainnet 7750 129700


unsafeMkInput :: Word32 -> Text -> Input Shelley
unsafeMkInput ix str = fromJust $ mkInput ix (unsafeB16 str)

unsafeMkOutput :: Natural -> Text -> Output Shelley
unsafeMkOutput n str = fromJust $ mkOutput n (unsafeBech32 str)

unsafeB16 :: Text -> ByteString
unsafeB16 = fromMaybe (error msg) . fromBase16
  where msg = "unable to decode base16 string."

unsafeBech32 :: Text -> ByteString
unsafeBech32 = fromMaybe (error msg) . fromBech32
  where msg = "unable to decode bech32 string."

unsafeB58 :: Text -> ByteString
unsafeB58 = fromMaybe (error msg) . fromBase58
  where msg = "unable to decode base58 string."

unsafeMkOutputFromBS :: Natural -> ByteString -> Output Shelley
unsafeMkOutputFromBS n bytes = fromJust $ mkOutput n bytes

unsafeMkShelleySignKeyFromBS :: ByteString -> SignKey Shelley
unsafeMkShelleySignKeyFromBS bytes = fromJust $ mkShelleySignKey bytes

unsafeMkByronSignKeyFromBS :: Text -> ByteString -> SignKey Shelley
unsafeMkByronSignKeyFromBS address witness =
    let
        Just attrs = mkAddrAttributes (unsafeB58 address)
        Just skey  = mkByronSignKey attrs witness
    in
        skey

unsafeMkInputFromBS :: Word32 -> ByteString -> Input Shelley
unsafeMkInputFromBS ix bytes = fromJust $ mkInput ix bytes

--
-- Test Vectors
--

txidTxt :: Text
txidTxt = "46ba24e81def97dd70971bd0e2bd93176def634a5140280a1667df57c0b6fe57"

addrTxt :: Text
addrTxt =
    "addr1q9wteuqmnkywcz9zefjyp0wn3tctz9xgxm8fpcmyzn9uypknudck0fzve4346yytz3wpw\
    \v9yhlxt7jwuc7ytwx2vfkyq75lt2a"

addr :: Word8 -> ByteString
addr b = BS.pack $ 1 : replicate 64 b

byronAddrs :: [Text]
byronAddrs =
    -- Mainnet
    [ "Ae2tdPwUPEZETXfbQxKMkMJQY1MoHCBS7bkw6TmhLjRvi9LZh1uDnXy319f"
    , "Ae2tdPwUPEZ69HTPqLpnFFw2MAfwdEoV5cpVQP5Uy1bPijSEHQmMXUfT3q5"

    -- Testnet
    , "2cWKMJemoBajc46Wu4Z7e6bG48myZWfB7Z6pD77L6PrJQWt9HZ3Yv7o8CYZTBMqHTPTkv"
    , "2cWKMJemoBaiLiNB8QpHKjkQhnPdQSyxaLb8JJFUQYpiVzgEJE59fN7V7StqnyDuDjHYJ"
    ]

txid :: ByteString
txid = BS.pack $ replicate 32 0

wit :: Word8 -> ByteString
wit b = BS.pack $ replicate 96 b
