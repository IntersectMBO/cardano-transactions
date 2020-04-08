{-# LANGUAGE OverloadedStrings #-}

module Data.UTxO.Transaction.Cardano.ByronSpec
    ( spec
    ) where

import Prelude

import Cardano.Chain.Genesis
    ( mainnetProtocolMagicId )
import Cardano.Crypto.ProtocolMagic
    ( ProtocolMagicId (..) )
import Data.ByteString
    ( ByteString )
import Data.Function
    ( (&) )
import Data.Maybe
    ( fromMaybe )
import Data.Text
    ( Text )
import Data.UTxO.Transaction.Cardano.Byron
    ( fromBase16, fromBase58, mkInput, mkOutput, mkSignKey )
import Test.Hspec
    ( Spec, describe, it, shouldBe )
import Test.QuickCheck
    ( Property, conjoin, counterexample, property, (===) )

import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Pretty as CBOR
import qualified Data.ByteString.Char8 as B8
import qualified Data.UTxO.Transaction as Tx

spec :: Spec
spec = do
    describe "(Mainnet) Golden Tests Transaction Construction" $ do
        let Just out0 = mkOutput 42 $ unsafeB58
                "Ae2tdPwUPEZETXfbQxKMkMJQY1MoHCBS7bkw6TmhLjRvi9LZh1uDnXy319f"

        let Just out1 = mkOutput 14 $ unsafeB58
                "Ae2tdPwUPEZ69HTPqLpnFFw2MAfwdEoV5cpVQP5Uy1bPijSEHQmMXUfT3q5"

        let Just out2 = mkOutput 14 $ unsafeB58
                "Ae2tdPwUPEZETXfbQxKMkMJQY1MoHCBS7bkw6TmhLjRvi9LZh1uDnXy319f"

        it "1 input, 1 output" $ do
            compareGolden goldenMainnet__1_1 $ Tx.empty mainnetProtocolMagicId
                & Tx.addInput inp0
                & Tx.addOutput out0
                & Tx.lock
                & Tx.signWith key0
                & Tx.serialize

        it "2 inputs, 2 outputs" $ do
            compareGolden goldenMainnet__2_2 $ Tx.empty mainnetProtocolMagicId
                & Tx.addInput inp0
                & Tx.addInput inp1
                & Tx.addOutput out0
                & Tx.addOutput out1
                & Tx.lock
                & Tx.signWith key0
                & Tx.signWith key1
                & Tx.serialize

        it "1 input, 25 outputs" $ do
            compareGolden goldenMainnet__25_1 $ Tx.empty mainnetProtocolMagicId
                & Tx.addInput inp0
                & flip (foldr Tx.addOutput) (replicate 25 out2)
                & Tx.lock
                & Tx.signWith key0
                & Tx.serialize

    describe "(Testnet) Golden Tests Transaction Construction" $ do
        let Just out0 = mkOutput 42 $ unsafeB58
                "2cWKMJemoBajc46Wu4Z7e6bG48myZWfB7Z6pD77L6PrJQWt9HZ3Yv7o8CYZTBMqHTPTkv"

        let Just out1 = mkOutput 14 $ unsafeB58
                "2cWKMJemoBaiLiNB8QpHKjkQhnPdQSyxaLb8JJFUQYpiVzgEJE59fN7V7StqnyDuDjHYJ"

        let Just out2 = mkOutput 14 $ unsafeB58
                "2cWKMJemoBajc46Wu4Z7e6bG48myZWfB7Z6pD77L6PrJQWt9HZ3Yv7o8CYZTBMqHTPTkv"

        it "1 input, 1 output" $ do
            compareGolden goldenTestnet__1_1 $ Tx.empty testnetProtocolMagicId
                & Tx.addInput inp0
                & Tx.addOutput out0
                & Tx.lock
                & Tx.signWith key0
                & Tx.serialize

        it "2 inputs, 2 outputs" $ do
            compareGolden goldenTestnet__2_2 $ Tx.empty testnetProtocolMagicId
                & Tx.addInput inp0
                & Tx.addInput inp1
                & Tx.addOutput out0
                & Tx.addOutput out1
                & Tx.lock
                & Tx.signWith key0
                & Tx.signWith key1
                & Tx.serialize

        it "1 input, 25 outputs" $ do
            compareGolden goldenTestnet__25_1 $ Tx.empty testnetProtocolMagicId
                & Tx.addInput inp0
                & flip (foldr Tx.addOutput) (replicate 25 out2)
                & Tx.lock
                & Tx.signWith key0
                & Tx.serialize
  where
    Just inp0 = mkInput 0 $ unsafeB16
        "3b40265111d8bb3c3c608d95b3a0bf83461ace32d79336579a1939b3aad1c0b7"

    Just inp1 = mkInput 1 $ unsafeB16
        "3b40265111d8bb3c3c608d95b3a0bf83461ace32d79336579a1939b3aad1c0b7"

    Just key0 = mkSignKey $ unsafeB16
        "e0860dab46f13e74ab834142e8877b80bf22044cae8ebab7a21ed1b8dc00c155\
        \f6b78eee2a5bbd453ce7e7711b2964abb6a36837e475271f18ff36ae5fc8af73\
        \e25db39fb78e74d4b53fb51776d0f5eb360e62d09b853f3a87ac25bf834ee1fb"

    Just key1 = mkSignKey $ unsafeB16
        "00bc08703e4bb4d9fe6edf4ad2ccb1ffc6fb8d555d96805d643e5d06afde605c\
        \e7d41d056def11417b17d3fb7415392b5f329877f372e55b0959c71b2bd2b447\
        \1bbf1c6081545b1ab140578d7b5c035bf904d05dd8e9b79b34d3160f86206bfc"

compareGolden
    :: ByteString
        -- ^ An expected encoded result
    -> Either e ByteString
        -- ^ The actual string received
    -> Property
compareGolden want (Right got) =
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

unsafeB16 :: Text -> ByteString
unsafeB16 = fromMaybe (error msg) . fromBase16
  where msg = "unable to decode base16 string."

unsafeB58 :: Text -> ByteString
unsafeB58 = fromMaybe (error msg) . fromBase58
  where msg = "unable to decode base58 string."

testnetProtocolMagicId :: ProtocolMagicId
testnetProtocolMagicId = ProtocolMagicId 1097911063

--
-- Test Vectors
--

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