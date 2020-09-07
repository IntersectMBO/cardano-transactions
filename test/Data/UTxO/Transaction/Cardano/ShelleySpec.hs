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
    ( fromBase16, fromBase58, fromBech32 )
import Data.UTxO.Transaction.Cardano.Shelley
    ( NetworkId (..)
    , Shelley
    , mkByronSignKey
    , mkInit
    , mkInput
    , mkOutput
    , mkShelleySignKey
    )
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

        it "Missing Signature" $ do
            let result = Tx.empty dummyInit
                       & Tx.addInput (unsafeMkInput 0 (txids !! 0))
                       & Tx.addOutput (unsafeMkOutput 1400000 (addrs !! 0))
                       & Tx.lock
                       & Tx.serialize
            result `shouldBe` Left MissingSignature

    describe "(integration tests) Transaction Construction" $ do
        it "1 input, 1 output (DSL) - Shelley witnesses" $ do
            compareIntegrationTesting integrationTesting__1_1 $ Tx.empty dummyInit
                & Tx.addInput (unsafeMkInput 0 (txids !! 0))
                & Tx.addOutput (unsafeMkOutput (10000000 - 2000000 - 129700) (addrs !! 1))
                & Tx.addOutput (unsafeMkOutput 2000000 (addrs !! 2))
                & Tx.lock
                & Tx.signWith (unsafeMkShelleySignKey (keys !! 0))
                & Tx.serialize
        it "1 input, 1 output (DSL) - Byron witnesses" $ do
            compareIntegrationTesting integrationTesting__1_1_byron $ Tx.empty dummyInit'
                & Tx.addInput (unsafeMkInput 0 (txids !! 0))
                & Tx.addOutput (unsafeMkOutput' (10000000 - 1000000 - 138200) (byronAddrs !! 0))
                & Tx.addOutput (unsafeMkOutput 1000000 (addrs !! 2))
                & Tx.lock
                & Tx.signWith (uncurry unsafeMkByronSignKey (byronKeys !! 0))
                & Tx.serialize
        it "1 input, 1 output (DSL) - Shelley witnesses" $ do
            let addr2 = BS.pack $ 1 : replicate 64 1
            let addr3 = BS.pack $ 1 : replicate 64 2
            let txId = BS.pack $ replicate 32 0
            let amtInp = 10000000
            let amtFee = 129700
            let amtOut = 2000000
            let amtChange = amtInp - amtOut - amtFee
            let wit = BS.pack $ replicate 96 0

            compareIntegrationTesting integrationTesting $ Tx.empty (mkInit Mainnet 7750 (fromIntegral amtFee))
                & Tx.addInput (unsafeMkInput1 0 txId)
                & Tx.addOutput (unsafeMkOutput1 amtOut addr2)
                & Tx.addOutput (unsafeMkOutput1 amtChange addr3)
                & Tx.lock
                & Tx.signWith (unsafeMkShelleySignKey1 wit)
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
    \cc788b7194c4d881a0078175c825839011f4595045be843e5eca0cc3be5757a\
    \ed6c9fe77d97b64c13ea50005e40d8e350a139670d5dc3e0ad7ac00646d96ee\
    \513640709a1708f98761a001e8480021a0001faa403191e46a10081825820c5\
    \b31478ced8c20ea3349f4833d12db2068483a96e3d77d5ccd942046cbeeca25\
    \840087987af97bdab10b9eb0ba5dd659f9eb58b8515c3ae8f63c799d9369f09\
    \2bd739449e4c63f867dcbd91f8e4c7dbdb0b70519c3fee0b9349cb6b066acff\
    \8c501f6"

integrationTesting__1_1_byron :: ByteString
integrationTesting__1_1_byron = unsafeB16
    "83a4008182582046ba24e81def97dd70971bd0e2bd93176def634a5140280a1\
    \667df57c0b6fe5700018282584c82d818584283581cb8cab01f3117517f219d\
    \fe927c1339c718445422df246ac05b353e72a101581e581c2af08478866f967\
    \927478899deb36c2f9b6713acd719672dc7b9e595001ab8705ac91a00873868\
    \825839011f4595045be843e5eca0cc3be5757aed6c9fe77d97b64c13ea50005\
    \e40d8e350a139670d5dc3e0ad7ac00646d96ee513640709a1708f98761a000f\
    \4240021a00021bd803191e46a10281845820d2a1098573423204e6c3ac10a76\
    \02f9b01aa32a5b65b8c75503450fc3b3ea317584022287a7079066cb5f4bb29\
    \a6ec4f2f9f58a67cb0cc788cf58f78f75b2a179fd4c048bf472705ccaa10953\
    \fa8b2c615470933543f02df6d521c4d98443747290a582006c552df3d160132\
    \01a44025f79b7eae1cd697b3fb173f31110b97935c6ba6005822a101581e581\
    \c2af08478866f967927478999c5a7e904b22c68abaf73cabeb0e6c400f6"

integrationTesting :: ByteString
integrationTesting = unsafeB16
    "83a400818258200000000000000000000000000000000000000000000000000000\
    \000000000000000182825839010101010101010101010101010101010101010101\
    \010101010101010101010101010101010101010101010101010101010101010101\
    \0101011a001e848082583901020202020202020202020202020202020202020202\
    \020202020202020202020202020202020202020202020202020202020202020202\
    \02021a0078175c021a0001faa403191e46a1008182582001000000000000000000\
    \000000000000000000000000000000000000000000005840d7af60ae33d2af3514\
    \11c1445c79590526990bfa73cbb3732b54ef322daa142e6884023410f8be3c16e9\
    \bd52076f2bb36bf38dfe034a9f04658e9f56197ab80ff6"

--
-- Internal
--
dummyInit :: Init Shelley
dummyInit = mkInit Mainnet 7750 129700

dummyInit' :: Init Shelley
dummyInit' = mkInit Mainnet 7750 138200

unsafeMkInput :: Word32 -> Text -> Input Shelley
unsafeMkInput ix str = fromJust $ mkInput ix (unsafeB16 str)

unsafeMkOutput :: Natural -> Text -> Output Shelley
unsafeMkOutput n str = fromJust $ mkOutput n (unsafeBech32 str)

unsafeMkOutput' :: Natural -> Text -> Output Shelley
unsafeMkOutput' n str = fromJust $ mkOutput n (unsafeBase58 str)

unsafeMkShelleySignKey :: Text -> SignKey Shelley
unsafeMkShelleySignKey str = fromJust $ mkShelleySignKey (unsafeB16 str)

unsafeMkByronSignKey :: Text -> Text -> SignKey Shelley
unsafeMkByronSignKey str1 str2 =
    let addr = unsafeBase58 str1
    in fromJust $ mkByronSignKey addr (unsafeB16 str2)

unsafeB16 :: Text -> ByteString
unsafeB16 = fromMaybe (error msg) . fromBase16
  where msg = "unable to decode base16 string."

unsafeBech32 :: Text -> ByteString
unsafeBech32 = fromMaybe (error msg) . fromBech32
  where msg = "unable to decode bech32 string."

unsafeBase58 :: Text -> ByteString
unsafeBase58 = fromMaybe (error msg) . fromBase58
  where msg = "unable to decode base58 string."

unsafeMkOutput1 :: Natural -> ByteString -> Output Shelley
unsafeMkOutput1 n bytes = fromJust $ mkOutput n bytes

unsafeMkShelleySignKey1 :: ByteString -> SignKey Shelley
unsafeMkShelleySignKey1 bytes = fromJust $ mkShelleySignKey bytes

unsafeMkInput1 :: Word32 -> ByteString -> Input Shelley
unsafeMkInput1 ix bytes = fromJust $ mkInput ix bytes

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

byronAddrs :: [Text]
byronAddrs =
    [ "DdzFFzCqrht3xS8ySv9t4MVM1euWvcC6WbzPZKkr6WqE7zcW8nSS9A3eGvcrMKvANGF6auayauFQjazrQtC7T8hx9CvXKq4U3qS2ApAC"
    ]
keys :: [Text]
keys =
    -- witness for input of txid !! 0 with ix = 0
    [ "400cd8a0c260e5aafe1768bf077815351fedf1bb698e1fb4a32903d52f006541\
      \c9dfe5b6c62f5c30aa958711233696d2d3f8486547a9e2f928d064f5752acfa8\
      \1d028f8516adad09a8efe3a0209e7e75d6bc61265f48d7471acdd2bf7ee7b23d"
    ]

byronKeys :: [(Text,Text)]
byronKeys =
    [ ( "DdzFFzCqrhsfRTAFKYtEMjB1vy5fTth3QEitVbMBuk5r9Um6Uf2bWYj8cSYfbad9MLKokM2Y5FhybrJqCgUDVPNPkgG6oa33VLQ6jugc"
      , "c323bec83ccf2e39ee42d499acd5ee7ade10e822270b4479f8e98c8c8e8abf0842dc7d\
        \e2c0e69e52982743c73a75ea06ef87c80b94d380c83a9a99060488438f06c552df3d16\
        \013201a44025f79b7eae1cd697b3fb173f31110b97935c6ba600"
      )
    ]
