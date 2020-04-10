{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.UTxO.Transaction.Cardano.ByronSpec
    ( spec
    ) where

import Prelude

import Cardano.Chain.UTxO
    ( TxIn (..), TxInWitness (..), TxOut (..), TxSigData (..) )
import Cardano.Crypto.ProtocolMagic
    ( ProtocolMagicId (..) )
import Control.Monad
    ( foldM )
import Data.ByteString
    ( ByteString )
import Data.Function
    ( (&) )
import Data.List.NonEmpty
    ( NonEmpty )
import Data.Maybe
    ( fromJust, fromMaybe )
import Data.Text
    ( Text )
import Data.UTxO.Transaction
    ( ErrMkPayment (..), MkPayment (..) )
import Data.UTxO.Transaction.Cardano.Byron
    ( Byron
    , decodeCoinSel
    , decodeTx
    , encodeCoinSel
    , encodeTx
    , fromBase16
    , fromBase58
    , fromBase64
    , mainnetMagic
    , mkInit
    , mkInput
    , mkOutput
    , mkSignKey
    , testnetMagic
    )
import Data.Word
    ( Word32 )
import Numeric.Natural
    ( Natural )
import System.Process
    ( readProcess )
import Test.Cardano.Chain.UTxO.Gen
    ( genTxIn, genTxInWitness, genTxOut, genTxSigData )
import Test.Hspec
    ( Spec, describe, expectationFailure, it, shouldBe )
import Test.QuickCheck
    ( Arbitrary (..)
    , NonEmptyList (..)
    , Property
    , conjoin
    , counterexample
    , elements
    , listOf
    , property
    , quickCheck
    , withMaxSuccess
    , (===)
    )
import Test.QuickCheck.Hedgehog
    ( hedgehog )

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Pretty as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteString.Lazy as BL
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.UTxO.Transaction as Tx

{-# ANN spec ("HLint: ignore Use head" :: String) #-}

spec :: Spec
spec = do
    describe "CBOR roundtrips" $ do
        it "CoinSel Byron" $ withMaxSuccess 500 $ property $
            prop_RoundTripCBOR encodeCoinSel decodeCoinSel

        it "Tx Byron" $ withMaxSuccess 500 $ property $
            prop_RoundTripCBOR encodeTx decodeTx

    describe "(Mainnet) Golden Tests Transaction Construction" $ do
        it "1 input, 1 output (DSL)" $ do
            compareGolden goldenMainnet__1_1 $ Tx.empty mainnetMagic
                & Tx.addInput (unsafeMkInput 0 (txids !! 0))
                & Tx.addOutput (unsafeMkOutput 42 (addrs !! 0))
                & Tx.lock
                & Tx.signWith (unsafeMkSignKey (keys !! 0))
                & Tx.serialize

        it "1 input, 1 output (CLI)" $ do
            state <- cardanoTx [ "empty", mainnetMagicT ] ""
                 >>= cardanoTx [ "add-input", "0", txids !! 0 ]
                 >>= cardanoTx [ "add-output", "42", addrs !! 0 ]
                 >>= cardanoTx [ "lock" ]
                 >>= cardanoTx [ "sign-with", keys !! 0 ]

            defaul <- fromBase64E <$> cardanoTx [ "serialize" ] state
            base16 <- fromBase16E <$> cardanoTx [ "serialize", "--base16" ] state
            base64 <- fromBase64E <$> cardanoTx [ "serialize", "--base64" ] state
            compareGolden goldenMainnet__1_1 base16
            compareGolden goldenMainnet__1_1 base64
            compareGolden goldenMainnet__1_1 defaul

        it "2 inputs, 2 outputs (DSL)" $ do
            compareGolden goldenMainnet__2_2 $ Tx.empty mainnetMagic
                & Tx.addInput (unsafeMkInput 0 (txids !! 0))
                & Tx.addInput (unsafeMkInput 1 (txids !! 0))
                & Tx.addOutput (unsafeMkOutput 42 (addrs !! 0))
                & Tx.addOutput (unsafeMkOutput 14 (addrs !! 1))
                & Tx.lock
                & Tx.signWith (unsafeMkSignKey (keys !! 0))
                & Tx.signWith (unsafeMkSignKey (keys !! 1))
                & Tx.serialize

        it "2 inputs, 2 outputs (CLI)" $ do
            state <- cardanoTx [ "empty", mainnetMagicT ] ""
                 >>= cardanoTx [ "add-input", "0", txids !! 0 ]
                 >>= cardanoTx [ "add-input", "1", txids !! 0 ]
                 >>= cardanoTx [ "add-output", "42", addrs !! 0 ]
                 >>= cardanoTx [ "add-output", "14", addrs !! 1 ]
                 >>= cardanoTx [ "lock" ]
                 >>= cardanoTx [ "sign-with", keys !! 0 ]
                 >>= cardanoTx [ "sign-with", keys !! 1 ]

            defaul <- fromBase64E <$> cardanoTx [ "serialize" ] state
            base16 <- fromBase16E <$> cardanoTx [ "serialize", "--base16" ] state
            base64 <- fromBase64E <$> cardanoTx [ "serialize", "--base64" ] state
            compareGolden goldenMainnet__2_2 base16
            compareGolden goldenMainnet__2_2 base64
            compareGolden goldenMainnet__2_2 defaul

        it "1 input, 25 outputs (DSL)" $ do
            compareGolden goldenMainnet__25_1 $ Tx.empty mainnetMagic
                & Tx.addInput (unsafeMkInput 0 (txids !! 0))
                & flip (foldr Tx.addOutput) (replicate 25 $ unsafeMkOutput 14 (addrs !! 0))
                & Tx.lock
                & Tx.signWith (unsafeMkSignKey (keys !! 0))
                & Tx.serialize

        it "1 input, 25 outputs (CLI)" $ do
            state <- cardanoTx [ "empty", mainnetMagicT ] ""
                 >>= cardanoTx [ "add-input", "0", txids !! 0 ]
                 >>= flip (foldM (&)) (replicate 25 $ cardanoTx [ "add-output", "14", addrs !! 0])
                 >>= cardanoTx [ "lock" ]
                 >>= cardanoTx [ "sign-with", keys !! 0 ]

            defaul <- fromBase64E <$> cardanoTx [ "serialize" ] state
            base16 <- fromBase16E <$> cardanoTx [ "serialize", "--base16" ] state
            base64 <- fromBase64E <$> cardanoTx [ "serialize", "--base64" ] state
            compareGolden goldenMainnet__25_1 base16
            compareGolden goldenMainnet__25_1 base64
            compareGolden goldenMainnet__25_1 defaul

    describe "(Testnet) Golden Tests Transaction Construction" $ do
        it "1 input, 1 output (DSL)" $ do
            compareGolden goldenTestnet__1_1 $ Tx.empty testnetMagic
                & Tx.addInput (unsafeMkInput 0 (txids !! 0))
                & Tx.addOutput (unsafeMkOutput 42 (addrs !! 2))
                & Tx.lock
                & Tx.signWith (unsafeMkSignKey (keys !! 0))
                & Tx.serialize

        it "1 input, 1 output (CLI)" $ do
            state <- cardanoTx [ "empty", testnetMagicT ] ""
                 >>= cardanoTx [ "add-input", "0", txids !! 0 ]
                 >>= cardanoTx [ "add-output", "42", addrs !! 2 ]
                 >>= cardanoTx [ "lock" ]
                 >>= cardanoTx [ "sign-with", keys !! 0 ]

            defaul <- fromBase64E <$> cardanoTx [ "serialize" ] state
            base16 <- fromBase16E <$> cardanoTx [ "serialize", "--base16" ] state
            base64 <- fromBase64E <$> cardanoTx [ "serialize", "--base64" ] state
            compareGolden goldenTestnet__1_1 base16
            compareGolden goldenTestnet__1_1 base64
            compareGolden goldenTestnet__1_1 defaul


        it "2 inputs, 2 outputs (DSL)" $ do
            compareGolden goldenTestnet__2_2 $ Tx.empty testnetMagic
                & Tx.addInput (unsafeMkInput 0 (txids !! 0))
                & Tx.addInput (unsafeMkInput 1 (txids !! 0))
                & Tx.addOutput (unsafeMkOutput 42 (addrs !! 2))
                & Tx.addOutput (unsafeMkOutput 14 (addrs !! 3))
                & Tx.lock
                & Tx.signWith (unsafeMkSignKey (keys !! 0))
                & Tx.signWith (unsafeMkSignKey (keys !! 1))
                & Tx.serialize

        it "2 inputs, 2 outputs (CLI)" $ do
            state <- cardanoTx [ "empty", testnetMagicT ] ""
                 >>= cardanoTx [ "add-input", "0", txids !! 0 ]
                 >>= cardanoTx [ "add-input", "1", txids !! 0 ]
                 >>= cardanoTx [ "add-output", "42", addrs !! 2 ]
                 >>= cardanoTx [ "add-output", "14", addrs !! 3 ]
                 >>= cardanoTx [ "lock" ]
                 >>= cardanoTx [ "sign-with", keys !! 0 ]
                 >>= cardanoTx [ "sign-with", keys !! 1 ]

            defaul <- fromBase64E <$> cardanoTx [ "serialize" ] state
            base16 <- fromBase16E <$> cardanoTx [ "serialize", "--base16" ] state
            base64 <- fromBase64E <$> cardanoTx [ "serialize", "--base64" ] state
            compareGolden goldenTestnet__2_2 base16
            compareGolden goldenTestnet__2_2 base64
            compareGolden goldenTestnet__2_2 defaul

        it "1 input, 25 outputs (DSL)" $ do
            compareGolden goldenTestnet__25_1 $ Tx.empty testnetMagic
                & Tx.addInput (unsafeMkInput 0 (txids !! 0))
                & flip (foldr Tx.addOutput) (replicate 25 $ unsafeMkOutput 14 (addrs !! 2))
                & Tx.lock
                & Tx.signWith (unsafeMkSignKey (keys !! 0))
                & Tx.serialize


        it "1 input, 25 outputs (CLI)" $ do
            state <- cardanoTx [ "empty", testnetMagicT ] ""
                 >>= cardanoTx [ "add-input", "0", txids !! 0 ]
                 >>= flip (foldM (&)) (replicate 25 $ cardanoTx [ "add-output", "14", addrs !! 2])
                 >>= cardanoTx [ "lock" ]
                 >>= cardanoTx [ "sign-with", keys !! 0 ]

            defaul <- fromBase64E <$> cardanoTx [ "serialize" ] state
            base16 <- fromBase16E <$> cardanoTx [ "serialize", "--base16" ] state
            base64 <- fromBase64E <$> cardanoTx [ "serialize", "--base64" ] state
            compareGolden goldenTestnet__25_1 base16
            compareGolden goldenTestnet__25_1 base64
            compareGolden goldenTestnet__25_1 defaul


    describe "Negative tests" $ do
        it "Missing Input" $ do
            let result = Tx.empty mainnetMagic
                       & Tx.addOutput (unsafeMkOutput 14 (addrs !! 0))
                       & Tx.lock
                       & Tx.serialize
            result `shouldBe` Left MissingInput

        it "Missing Output" $ do
            let result = Tx.empty mainnetMagic
                       & Tx.addInput (unsafeMkInput 0 (txids !! 0))
                       & Tx.lock
                       & Tx.serialize
            result `shouldBe` Left MissingOutput

        it "Missing Signature" $ do
            let result = Tx.empty mainnetMagic
                       & Tx.addInput (unsafeMkInput 0 (txids !! 0))
                       & Tx.addOutput (unsafeMkOutput 14 (addrs !! 0))
                       & Tx.lock
                       & Tx.serialize
            result `shouldBe` Left MissingSignature

--
-- Property: Roundtrip CBOR
--

prop_RoundTripCBOR
    :: (Show a, Eq a)
    => (a -> CBOR.Encoding)
    -> (forall s. CBOR.Decoder s a)
    -> a
    -> Property
prop_RoundTripCBOR encode decode a =
    result === Right (mempty, a)
  where
    bytes  = CBOR.toStrictByteString (encode a)
    result = CBOR.deserialiseFromBytes decode (BL.fromStrict bytes)

instance Arbitrary ProtocolMagicId where
    arbitrary = mkInit <$> arbitrary

instance Arbitrary TxIn where
    arbitrary = hedgehog genTxIn

instance Arbitrary TxOut where
    arbitrary = hedgehog genTxOut

instance Arbitrary TxSigData where
    arbitrary = hedgehog genTxSigData

instance Arbitrary ErrMkPayment where
    arbitrary = elements [ MissingInput, MissingOutput, MissingSignature ]

instance {-# OVERLAPS #-}
    ( Arbitrary a, Arbitrary b, Arbitrary c)
    => Arbitrary (ProtocolMagicId, NonEmpty a, NonEmpty b, c, [TxInWitness])
  where
    arbitrary = do
        pm   <- arbitrary
        wits <- listOf (hedgehog $ genTxInWitness pm)
        (,,,,)
            <$> pure pm
            <*> fmap (NE.fromList . getNonEmpty) arbitrary
            <*> fmap (NE.fromList . getNonEmpty) arbitrary
            <*> arbitrary
            <*> pure wits

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
compareGolden want (Right got) = quickCheck $ withMaxSuccess 1 $
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

--
-- Internal
--

unsafeMkInput :: Word32 -> Text -> Input Byron
unsafeMkInput ix str = fromJust $ mkInput ix (unsafeB16 str)

unsafeMkOutput :: Natural -> Text -> Output Byron
unsafeMkOutput n str = fromJust $ mkOutput n (unsafeB58 str)

unsafeMkSignKey :: Text -> SignKey Byron
unsafeMkSignKey str = fromJust $ mkSignKey (unsafeB16 str)

unsafeB16 :: Text -> ByteString
unsafeB16 = fromMaybe (error msg) . fromBase16
  where msg = "unable to decode base16 string."

unsafeB58 :: Text -> ByteString
unsafeB58 = fromMaybe (error msg) . fromBase58
  where msg = "unable to decode base58 string."

fromBase64E :: Text -> Either String ByteString
fromBase64E = maybe (Left msg) Right . fromBase64
  where msg = "unable to decode base64 string."

fromBase16E :: Text -> Either String ByteString
fromBase16E = maybe (Left msg) Right . fromBase16
  where msg = "unable to decode base16 string."

mainnetMagicT :: Text
mainnetMagicT = T.pack $ show $ unProtocolMagicId mainnetMagic

testnetMagicT :: Text
testnetMagicT = T.pack $ show $ unProtocolMagicId testnetMagic

cardanoTx
    :: [Text]
        -- ^ Arguments
    -> Text
        -- ^ stdin
    -> IO Text
        -- ^ stdout
cardanoTx args =
    fmap T.pack . readProcess "cardano-tx" (T.unpack <$> args) . T.unpack

--
-- Test Vectors
--

txids :: [Text]
txids =
    [ "3b40265111d8bb3c3c608d95b3a0bf83461ace32d79336579a1939b3aad1c0b7"
    ]

addrs :: [Text]
addrs =
    -- Mainnet
    [ "Ae2tdPwUPEZETXfbQxKMkMJQY1MoHCBS7bkw6TmhLjRvi9LZh1uDnXy319f"
    , "Ae2tdPwUPEZ69HTPqLpnFFw2MAfwdEoV5cpVQP5Uy1bPijSEHQmMXUfT3q5"

    -- Testnet
    , "2cWKMJemoBajc46Wu4Z7e6bG48myZWfB7Z6pD77L6PrJQWt9HZ3Yv7o8CYZTBMqHTPTkv"
    , "2cWKMJemoBaiLiNB8QpHKjkQhnPdQSyxaLb8JJFUQYpiVzgEJE59fN7V7StqnyDuDjHYJ"
    ]

keys :: [Text]
keys =
    [ "e0860dab46f13e74ab834142e8877b80bf22044cae8ebab7a21ed1b8dc00c155\
      \f6b78eee2a5bbd453ce7e7711b2964abb6a36837e475271f18ff36ae5fc8af73\
      \e25db39fb78e74d4b53fb51776d0f5eb360e62d09b853f3a87ac25bf834ee1fb"

    , "00bc08703e4bb4d9fe6edf4ad2ccb1ffc6fb8d555d96805d643e5d06afde605c\
      \e7d41d056def11417b17d3fb7415392b5f329877f372e55b0959c71b2bd2b447\
      \1bbf1c6081545b1ab140578d7b5c035bf904d05dd8e9b79b34d3160f86206bfc"
    ]

goldenMainnet__1_1 :: ByteString
goldenMainnet__1_1 = unsafeB16
    "82839f8200d81858248258203b40265111d8bb3c3c608d95b3a0bf83461ace3\
    \2d79336579a1939b3aad1c0b700ff9f8282d818582183581cb0e693cbc97272\
    \bd42aacf3b443ecac68b96b4fa20159186a4a38883a0001a497c15c6182affa\
    \0818200d81858858258408e620235b2a427259cc1a1c63af9ef179cb3b6e396\
    \da182e01d65dd3ed7be94be25db39fb78e74d4b53fb51776d0f5eb360e62d09\
    \b853f3a87ac25bf834ee1fb58403684501a21346ba3c19a6dc3479180e80a14\
    \e5edc0ffb4b1469abe0332e7e1caf291d50cb8d2c47ee37f6b44b7fbfcc5a1b\
    \01bc262038e93969b9a728a2b3500"

goldenMainnet__2_2 :: ByteString
goldenMainnet__2_2 = unsafeB16
    "82839f8200d81858248258203b40265111d8bb3c3c608d95b3a0bf83461ace32\
    \d79336579a1939b3aad1c0b7008200d81858248258203b40265111d8bb3c3c60\
    \8d95b3a0bf83461ace32d79336579a1939b3aad1c0b701ff9f8282d818582183\
    \581cb0e693cbc97272bd42aacf3b443ecac68b96b4fa20159186a4a38883a000\
    \1a497c15c6182a8282d818582183581c5d9254c4b41dbdc10f9c3b4517630497\
    \6e5e210decc38792ab2b50c9a0001ab537acfc0effa0828200d8185885825840\
    \8e620235b2a427259cc1a1c63af9ef179cb3b6e396da182e01d65dd3ed7be94b\
    \e25db39fb78e74d4b53fb51776d0f5eb360e62d09b853f3a87ac25bf834ee1fb\
    \5840a214bef8191aca0b41c109101c420deb131f5eff63394285dc18fc39726f\
    \b24a7bc34adce5a6e234360aae398b431a47d9b9d9a71cdcea6bd21bac7ed816\
    \53068200d81858858258402cf36a269802da7cc0408308bda9a0015e7e819121\
    \17013c93edeefa9b62bbb61bbf1c6081545b1ab140578d7b5c035bf904d05dd8\
    \e9b79b34d3160f86206bfc58403cb09514f2d27f5dab76480ec010b4e89c0c50\
    \f583c00bca487e0a45643e4c2854842c57d87f93961efad7037be8de5100d0fc\
    \e2383e8a11e20cb9762731da0c"

goldenMainnet__25_1 :: ByteString
goldenMainnet__25_1 = unsafeB16
    "82839f8200d81858248258203b40265111d8bb3c3c608d95b3a0bf83461ace32\
    \d79336579a1939b3aad1c0b700ff9f8282d818582183581cb0e693cbc97272bd\
    \42aacf3b443ecac68b96b4fa20159186a4a38883a0001a497c15c60e8282d818\
    \582183581cb0e693cbc97272bd42aacf3b443ecac68b96b4fa20159186a4a388\
    \83a0001a497c15c60e8282d818582183581cb0e693cbc97272bd42aacf3b443e\
    \cac68b96b4fa20159186a4a38883a0001a497c15c60e8282d818582183581cb0\
    \e693cbc97272bd42aacf3b443ecac68b96b4fa20159186a4a38883a0001a497c\
    \15c60e8282d818582183581cb0e693cbc97272bd42aacf3b443ecac68b96b4fa\
    \20159186a4a38883a0001a497c15c60e8282d818582183581cb0e693cbc97272\
    \bd42aacf3b443ecac68b96b4fa20159186a4a38883a0001a497c15c60e8282d8\
    \18582183581cb0e693cbc97272bd42aacf3b443ecac68b96b4fa20159186a4a3\
    \8883a0001a497c15c60e8282d818582183581cb0e693cbc97272bd42aacf3b44\
    \3ecac68b96b4fa20159186a4a38883a0001a497c15c60e8282d818582183581c\
    \b0e693cbc97272bd42aacf3b443ecac68b96b4fa20159186a4a38883a0001a49\
    \7c15c60e8282d818582183581cb0e693cbc97272bd42aacf3b443ecac68b96b4\
    \fa20159186a4a38883a0001a497c15c60e8282d818582183581cb0e693cbc972\
    \72bd42aacf3b443ecac68b96b4fa20159186a4a38883a0001a497c15c60e8282\
    \d818582183581cb0e693cbc97272bd42aacf3b443ecac68b96b4fa20159186a4\
    \a38883a0001a497c15c60e8282d818582183581cb0e693cbc97272bd42aacf3b\
    \443ecac68b96b4fa20159186a4a38883a0001a497c15c60e8282d81858218358\
    \1cb0e693cbc97272bd42aacf3b443ecac68b96b4fa20159186a4a38883a0001a\
    \497c15c60e8282d818582183581cb0e693cbc97272bd42aacf3b443ecac68b96\
    \b4fa20159186a4a38883a0001a497c15c60e8282d818582183581cb0e693cbc9\
    \7272bd42aacf3b443ecac68b96b4fa20159186a4a38883a0001a497c15c60e82\
    \82d818582183581cb0e693cbc97272bd42aacf3b443ecac68b96b4fa20159186\
    \a4a38883a0001a497c15c60e8282d818582183581cb0e693cbc97272bd42aacf\
    \3b443ecac68b96b4fa20159186a4a38883a0001a497c15c60e8282d818582183\
    \581cb0e693cbc97272bd42aacf3b443ecac68b96b4fa20159186a4a38883a000\
    \1a497c15c60e8282d818582183581cb0e693cbc97272bd42aacf3b443ecac68b\
    \96b4fa20159186a4a38883a0001a497c15c60e8282d818582183581cb0e693cb\
    \c97272bd42aacf3b443ecac68b96b4fa20159186a4a38883a0001a497c15c60e\
    \8282d818582183581cb0e693cbc97272bd42aacf3b443ecac68b96b4fa201591\
    \86a4a38883a0001a497c15c60e8282d818582183581cb0e693cbc97272bd42aa\
    \cf3b443ecac68b96b4fa20159186a4a38883a0001a497c15c60e8282d8185821\
    \83581cb0e693cbc97272bd42aacf3b443ecac68b96b4fa20159186a4a38883a0\
    \001a497c15c60e8282d818582183581cb0e693cbc97272bd42aacf3b443ecac6\
    \8b96b4fa20159186a4a38883a0001a497c15c60effa0818200d8185885825840\
    \8e620235b2a427259cc1a1c63af9ef179cb3b6e396da182e01d65dd3ed7be94b\
    \e25db39fb78e74d4b53fb51776d0f5eb360e62d09b853f3a87ac25bf834ee1fb\
    \58402b01895a50681f18418da48bf6cf9d70ad465018bb9276f6f0aa9b58ff2d\
    \2f273d584e4f27e228cd57a6f96009d5c27525fd92647db708185e488713d40d\
    \d402"

goldenTestnet__1_1 :: ByteString
goldenTestnet__1_1 = unsafeB16
    "82839f8200d81858248258203b40265111d8bb3c3c608d95b3a0bf83461ace32d7\
    \9336579a1939b3aad1c0b700ff9f8282d818582883581c946480eb45fa900e84d1\
    \f25141589468ca1d38bbb4e6c3abec7d1ea0a102451a4170cb17001a16bbe0ab18\
    \2affa0818200d81858858258408e620235b2a427259cc1a1c63af9ef179cb3b6e3\
    \96da182e01d65dd3ed7be94be25db39fb78e74d4b53fb51776d0f5eb360e62d09b\
    \853f3a87ac25bf834ee1fb584081b55eff4aa66b01c339ba2ec04dbad9d14bd049\
    \f6e8ae46211a2870c839966bfc721ca80c3b71d3719e468dcd16c877ff68a3cff8\
    \887ac9f3546da99e6bba00"

goldenTestnet__2_2 :: ByteString
goldenTestnet__2_2 = unsafeB16
    "82839f8200d81858248258203b40265111d8bb3c3c608d95b3a0bf83461ace32d7\
    \9336579a1939b3aad1c0b7008200d81858248258203b40265111d8bb3c3c608d95\
    \b3a0bf83461ace32d79336579a1939b3aad1c0b701ff9f8282d818582883581c94\
    \6480eb45fa900e84d1f25141589468ca1d38bbb4e6c3abec7d1ea0a102451a4170\
    \cb17001a16bbe0ab182a8282d818582883581c48a08edee8a9f586263d5f4e2288\
    \ac1a6e33656bfa2570fb8618c5c4a102451a4170cb17001ab30d9d270effa08282\
    \00d81858858258408e620235b2a427259cc1a1c63af9ef179cb3b6e396da182e01\
    \d65dd3ed7be94be25db39fb78e74d4b53fb51776d0f5eb360e62d09b853f3a87ac\
    \25bf834ee1fb5840fb812923b3b5a15a9d7005ef0b2ef714ed188c48f0ec16a7ef\
    \5e3661f92aebb203d3717fafbb10b4de6bf4fa6ad064b5ad65434a449983741182\
    \1af434d0280f8200d81858858258402cf36a269802da7cc0408308bda9a0015e7e\
    \81912117013c93edeefa9b62bbb61bbf1c6081545b1ab140578d7b5c035bf904d0\
    \5dd8e9b79b34d3160f86206bfc5840a57a996c2a1185f117db5e21aed47c54eccf\
    \263bf1e978114b1a2731086b4e8d189700c918b1ce04cd77316a15fb22f3a2cbed\
    \7de16fb4fc83d4cc4b11a3860b"

goldenTestnet__25_1 :: ByteString
goldenTestnet__25_1 = unsafeB16
    "82839f8200d81858248258203b40265111d8bb3c3c608d95b3a0bf83461ace32d7\
    \9336579a1939b3aad1c0b700ff9f8282d818582883581c946480eb45fa900e84d1\
    \f25141589468ca1d38bbb4e6c3abec7d1ea0a102451a4170cb17001a16bbe0ab0e\
    \8282d818582883581c946480eb45fa900e84d1f25141589468ca1d38bbb4e6c3ab\
    \ec7d1ea0a102451a4170cb17001a16bbe0ab0e8282d818582883581c946480eb45\
    \fa900e84d1f25141589468ca1d38bbb4e6c3abec7d1ea0a102451a4170cb17001a\
    \16bbe0ab0e8282d818582883581c946480eb45fa900e84d1f25141589468ca1d38\
    \bbb4e6c3abec7d1ea0a102451a4170cb17001a16bbe0ab0e8282d818582883581c\
    \946480eb45fa900e84d1f25141589468ca1d38bbb4e6c3abec7d1ea0a102451a41\
    \70cb17001a16bbe0ab0e8282d818582883581c946480eb45fa900e84d1f2514158\
    \9468ca1d38bbb4e6c3abec7d1ea0a102451a4170cb17001a16bbe0ab0e8282d818\
    \582883581c946480eb45fa900e84d1f25141589468ca1d38bbb4e6c3abec7d1ea0\
    \a102451a4170cb17001a16bbe0ab0e8282d818582883581c946480eb45fa900e84\
    \d1f25141589468ca1d38bbb4e6c3abec7d1ea0a102451a4170cb17001a16bbe0ab\
    \0e8282d818582883581c946480eb45fa900e84d1f25141589468ca1d38bbb4e6c3\
    \abec7d1ea0a102451a4170cb17001a16bbe0ab0e8282d818582883581c946480eb\
    \45fa900e84d1f25141589468ca1d38bbb4e6c3abec7d1ea0a102451a4170cb1700\
    \1a16bbe0ab0e8282d818582883581c946480eb45fa900e84d1f25141589468ca1d\
    \38bbb4e6c3abec7d1ea0a102451a4170cb17001a16bbe0ab0e8282d81858288358\
    \1c946480eb45fa900e84d1f25141589468ca1d38bbb4e6c3abec7d1ea0a102451a\
    \4170cb17001a16bbe0ab0e8282d818582883581c946480eb45fa900e84d1f25141\
    \589468ca1d38bbb4e6c3abec7d1ea0a102451a4170cb17001a16bbe0ab0e8282d8\
    \18582883581c946480eb45fa900e84d1f25141589468ca1d38bbb4e6c3abec7d1e\
    \a0a102451a4170cb17001a16bbe0ab0e8282d818582883581c946480eb45fa900e\
    \84d1f25141589468ca1d38bbb4e6c3abec7d1ea0a102451a4170cb17001a16bbe0\
    \ab0e8282d818582883581c946480eb45fa900e84d1f25141589468ca1d38bbb4e6\
    \c3abec7d1ea0a102451a4170cb17001a16bbe0ab0e8282d818582883581c946480\
    \eb45fa900e84d1f25141589468ca1d38bbb4e6c3abec7d1ea0a102451a4170cb17\
    \001a16bbe0ab0e8282d818582883581c946480eb45fa900e84d1f25141589468ca\
    \1d38bbb4e6c3abec7d1ea0a102451a4170cb17001a16bbe0ab0e8282d818582883\
    \581c946480eb45fa900e84d1f25141589468ca1d38bbb4e6c3abec7d1ea0a10245\
    \1a4170cb17001a16bbe0ab0e8282d818582883581c946480eb45fa900e84d1f251\
    \41589468ca1d38bbb4e6c3abec7d1ea0a102451a4170cb17001a16bbe0ab0e8282\
    \d818582883581c946480eb45fa900e84d1f25141589468ca1d38bbb4e6c3abec7d\
    \1ea0a102451a4170cb17001a16bbe0ab0e8282d818582883581c946480eb45fa90\
    \0e84d1f25141589468ca1d38bbb4e6c3abec7d1ea0a102451a4170cb17001a16bb\
    \e0ab0e8282d818582883581c946480eb45fa900e84d1f25141589468ca1d38bbb4\
    \e6c3abec7d1ea0a102451a4170cb17001a16bbe0ab0e8282d818582883581c9464\
    \80eb45fa900e84d1f25141589468ca1d38bbb4e6c3abec7d1ea0a102451a4170cb\
    \17001a16bbe0ab0e8282d818582883581c946480eb45fa900e84d1f25141589468\
    \ca1d38bbb4e6c3abec7d1ea0a102451a4170cb17001a16bbe0ab0effa0818200d8\
    \1858858258408e620235b2a427259cc1a1c63af9ef179cb3b6e396da182e01d65d\
    \d3ed7be94be25db39fb78e74d4b53fb51776d0f5eb360e62d09b853f3a87ac25bf\
    \834ee1fb5840e0f9591bfd7b67738db6b689d514bba1fe8c730491f9bdad7fbe05\
    \58974e57c1ef62c331da2ad528ff829b5406638d384a0b4d0b40b378f4063acb73\
    \851d3906"
