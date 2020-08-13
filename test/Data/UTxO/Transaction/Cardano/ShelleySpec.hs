{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
    ( Spec, describe, expectationFailure, it, shouldBe )
import Test.Hspec.QuickCheck
    ( prop )
import Test.QuickCheck
    ( Arbitrary (..)
    , forAll
    , oneof
    , quickCheck
    , suchThat
    , withMaxSuccess
    , (===)
    )

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
                       & Tx.addOutput (unsafeMkOutput 14 (addrs !! 0))
                       & Tx.lock
                       & Tx.serialize
            result `shouldBe` Left MissingInput

        it "Missing Output" $ do
            let result = Tx.empty dummyInit
                       & Tx.addInput (unsafeMkInput 0 (txids !! 0))
                       & Tx.lock
                       & Tx.serialize
            result `shouldBe` Left MissingOutput

        it "Too low output" $ do
            let result = Tx.empty dummyInit
                       & Tx.addInput (unsafeMkInput 0 (txids !! 0))
                       & Tx.addOutput (unsafeMkOutput 14 (addrs !! 0))
                       & Tx.lock
                       & Tx.serialize
            result `shouldBe` Left TooLowOutput

        it "Missing Signature" $ do
            let result = Tx.empty dummyInit
                       & Tx.addInput (unsafeMkInput 0 (txids !! 0))
                       & Tx.addOutput (unsafeMkOutput 1400000 (addrs !! 0))
                       & Tx.lock
                       & Tx.serialize
            result `shouldBe` Left MissingSignature

    describe "(integration tests) Transaction Construction" $ do
        it "1 input, 1 output (DSL)" $ do
            compareIntegrationTesting integrationTesting__1_1 $ Tx.empty dummyInit
                & Tx.addInput (unsafeMkInput 0 (txids !! 0))
                & Tx.addOutput (unsafeMkOutput (10000000 - 129700) (addrs !! 1))
                & Tx.addOutput (unsafeMkOutput 2000000 (addrs !! 2))
                & Tx.lock
                & Tx.signWith (unsafeMkShelleySignKey (keys !! 0))
                & Tx.serialize
--
-- Input for integration testing - probably temporary
--

compareIntegrationTesting
    :: Show e
    => ByteString
        -- ^ An expected encoded result
    -> Either e ByteString
        -- ^ The actual string received
    -> IO ()
compareIntegrationTesting _want (Left e)   = expectationFailure (show e)
compareIntegrationTesting want (Right got) = quickCheck $ withMaxSuccess 1 $
    want === got

integrationTesting__1_1 :: ByteString
integrationTesting__1_1 = unsafeB16
    "83a4008182582046ba24e81def97dd70971bd0e2bd93176def634a5140280a1\
    \667df57c0b6fe5700018282583901c5dd8de847cd8f3613bb5c90bc282dfb8a\
    \3c582b868bd11b71b744f8d3e37167a44ccd635d108b145c1730a4bfccbf49d\
    \cc788b7194c4d881a00969bdc825839011f4595045be843e5eca0cc3be5757a\
    \ed6c9fe77d97b64c13ea50005e40d8e350a139670d5dc3e0ad7ac00646d96ee\
    \513640709a1708f98761a001e8480021a0001faa403191e46a10081825820c5\
    \b31478ced8c20ea3349f4833d12db2068483a96e3d77d5ccd942046cbeeca25\
    \840f7abe18faf480b0a3a6d95ba3dcca1f641d41eddf69c2de61eda544ecb8f\
    \9e8c015636120b0c8bcf07aae535ab16f495484e7c8992d9db3d855ae39d67a\
    \9900df6"

--
-- Internal
--
dummyInit :: Init Shelley
dummyInit = mkInit Mainnet 7750 129700

unsafeMkInput :: Word32 -> Text -> Input Shelley
unsafeMkInput ix str = fromJust $ mkInput ix (unsafeB16 str)

unsafeMkOutput :: Natural -> Text -> Output Shelley
unsafeMkOutput n str = fromJust $ mkOutput n (unsafeBech32 str)

unsafeMkShelleySignKey :: Text -> SignKey Shelley
unsafeMkShelleySignKey str = fromJust $ mkShelleySignKey (unsafeB16 str)

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
    -- the input !! 0 was created due to this tx
    [ "46ba24e81def97dd70971bd0e2bd93176def634a5140280a1667df57c0b6fe57"
    ]

addrs :: [Text]
addrs =
      -- input of txid !! 0 with ix = 0
    [ "addr1q9wteuqmnkywcz9zefjyp0wn3tctz9xgxm8fpcmyzn9uypknudck0fzve4346yytz3wpwv9yhlxt7jwuc7ytwx2vfkyq75lt2a"
      -- change
    , "addr1q8zamr0gglxc7dsnhdwfp0pg9hac50zc9wrgh5gmwxm5f7xnudck0fzve4346yytz3wpwv9yhlxt7jwuc7ytwx2vfkyquaaj4r"
     -- output
    , "addr1qy05t9gyt05y8e0v5rxrhet40tkke8l80ktmvnqnafgqqhjqmr34pgfevux4mslq44avqpjxm9hw2ymyquy6zuy0npmq030jj2"
    ]

keys :: [Text]
keys =
    -- witness for input of txid !! 0 with ix = 0
    [ "400cd8a0c260e5aafe1768bf077815351fedf1bb698e1fb4a32903d52f006541\
      \c9dfe5b6c62f5c30aa958711233696d2d3f8486547a9e2f928d064f5752acfa8\
      \1d028f8516adad09a8efe3a0209e7e75d6bc61265f48d7471acdd2bf7ee7b23d"
    ]
