{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.UTxO.Transaction.Cardano.ShelleySpec
    ( spec
    ) where

import Prelude

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
import Data.UTxO.Transaction.Cardano.Helpers
    ( fromBase16, fromBech32 )
import Data.UTxO.Transaction.Cardano.Shelley
    ( NetworkId (..), Shelley, mkInit, mkInput, mkOutput, mkShelleySignKey )
import Data.Word
    ( Word32, Word64 )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( Spec, describe, it, shouldBe )
import Test.Hspec.QuickCheck
    ( prop )
import Test.QuickCheck
    ( Arbitrary (..), forAll, oneof, suchThat )

import qualified Data.ByteString as BS
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
            let genInvalidAddress = BS.pack <$> arbitrary
            forAll genInvalidAddress (isNothing . mkOutput (fromIntegral coin))

        prop "Invalid SignKey" $ do
            let len = 96
            let genInvalidSignKey =
                    (BS.pack <$> arbitrary) `suchThat` ((/= len) . BS.length)
            forAll genInvalidSignKey (isNothing . mkShelleySignKey)

        it "Missing Input" $ do
            let result = Tx.empty (mkInit Mainnet 430000)
                       & Tx.addOutput (unsafeMkOutput 14 (addrs !! 0))
                       & Tx.lock
                       & Tx.serialize
            result `shouldBe` Left MissingInput

        it "Missing Output" $ do
            let result = Tx.empty (mkInit Mainnet 430000)
                       & Tx.addInput (unsafeMkInput 0 (txids !! 0))
                       & Tx.lock
                       & Tx.serialize
            result `shouldBe` Left MissingOutput

        it "Too low output" $ do
            let result = Tx.empty (mkInit Mainnet 430000)
                       & Tx.addInput (unsafeMkInput 0 (txids !! 0))
                       & Tx.addOutput (unsafeMkOutput 14 (addrs !! 0))
                       & Tx.lock
                       & Tx.serialize
            result `shouldBe` Left TooLowOutput

        it "Missing Signature" $ do
            let result = Tx.empty (mkInit Mainnet 430000)
                       & Tx.addInput (unsafeMkInput 0 (txids !! 0))
                       & Tx.addOutput (unsafeMkOutput 1400000 (addrs !! 0))
                       & Tx.lock
                       & Tx.serialize
            result `shouldBe` Left MissingSignature

--
-- Internal
--

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

--
-- Test Vectors
--

txids :: [Text]
txids =
    [ "3b40265111d8bb3c3c608d95b3a0bf83461ace32d79336579a1939b3aad1c0b7"
    ]

addrs :: [Text]
addrs =
    -- Testnet
    [ "addr1q9wteuqmnkywcz9zefjyp0wn3tctz9xgxm8fpcmyzn9uypknudck0fzve4346yytz3wpwv9yhlxt7jwuc7ytwx2vfkyq75lt2a"
    ]
