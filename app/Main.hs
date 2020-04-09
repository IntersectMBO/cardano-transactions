{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Prelude

import Control.Exception
    ( bracket )
import Control.Monad
    ( guard, void )
import Data.ByteArray.Encoding
    ( Base (..), convertFromBase, convertToBase )
import Data.ByteString
    ( ByteString )
import Data.Maybe
    ( isNothing )
import Data.Text
    ( Text )
import Data.UTxO.Transaction
    ( MkPayment (..) )
import Data.UTxO.Transaction.Cardano.Byron
    ( Byron
    , decodeCoinSel
    , decodeTx
    , encodeCoinSel
    , encodeTx
    , fromBase16
    , fromBase58
    , mkInit
    , mkInput
    , mkOutput
    , mkSignKey
    )
import Data.Word
    ( Word32 )
import Numeric.Natural
    ( Natural )
import Options.Applicative
    ( ArgumentFields
    , CommandFields
    , Mod
    , Parser
    , ParserInfo
    , argument
    , auto
    , command
    , customExecParser
    , footerDoc
    , header
    , headerDoc
    , helper
    , info
    , maybeReader
    , metavar
    , prefs
    , progDesc
    , showHelpOnEmpty
    , subparser
    )
import Options.Applicative.Help.Pretty
    ( indent, string, vsep )
import System.Console.ANSI
    ( Color (..)
    , ColorIntensity (..)
    , ConsoleLayer (..)
    , SGR (..)
    , hSetSGR
    , hSupportsANSIWithoutEmulation
    )
import System.Exit
    ( exitFailure )
import System.IO
    ( Handle, hIsTerminalDevice, stderr, stdin, stdout )

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as TIO
import qualified Data.UTxO.Transaction as Tx


main :: IO ()
main = setup >> parseCmd >>= \case
    CmdEmpty Empty{protocolMagic} -> do
        let state0 = Tx.empty protocolMagic
        hPutState stdout (encodeCoinSel state0)

    CmdAddInput AddInput{inputIx,inputTxId} -> do
        state <- hGetState stdin decodeCoinSel
        case mkInput inputIx inputTxId of
            Nothing -> failWith "Invalid input index or transaction id."
            Just input -> do
                let state' = Tx.addInput input state
                hPutState stdout (encodeCoinSel state')

    CmdAddOutput AddOutput{coin,address} -> do
        state <- hGetState stdin decodeCoinSel
        case mkOutput coin address of
            Nothing -> failWith "Invalid output value or address."
            Just output -> do
                let state' = Tx.addOutput output state
                hPutState stdout (encodeCoinSel state')

    CmdLock -> do
        state <- hGetState stdin decodeCoinSel
        let state' = Tx.lock state
        hPutState stdout (encodeTx state')

    CmdSignWith SignWith{prvKey} -> do
        state <- hGetState stdin decodeTx
        case mkSignKey prvKey of
            Nothing -> failWith "Invalid signing key."
            Just signKey -> do
                let state' = Tx.signWith signKey state
                hPutState stdout (encodeTx state')

    CmdSerialize -> do
        state <- hGetState stdin decodeTx
        case Tx.serialize state of
            Left e -> failWith (show e)
            Right bytes -> TIO.putStr (base64 bytes)
  where
    setup :: IO ()
    setup = do
        -- Enable ANSI colors on Windows.
        void $ hSupportsANSIWithoutEmulation stderr

parseCmd :: IO Cmd
parseCmd = customExecParser (prefs showHelpOnEmpty) cmd

data Cmd
    = CmdEmpty Empty
    | CmdAddInput AddInput
    | CmdAddOutput AddOutput
    | CmdLock
    | CmdSignWith SignWith
    | CmdSerialize
    deriving (Show)

cmd :: ParserInfo Cmd
cmd = info (helper <*> cmds) $ progDesc "cardano-tx"
  where
    cmds = subparser $ mconcat
        [ cmdEmpty
        , cmdAddInput
        , cmdAddOutput
        , cmdLock
        , cmdSignWith
        , cmdSerialize
        ]

--                       _
--                      | |
--   ___ _ __ ___  _ __ | |_ _   _
--  / _ \ '_ ` _ \| '_ \| __| | | |
-- |  __/ | | | | | |_) | |_| |_| |
--  \___|_| |_| |_| .__/ \__|\__, |
--                | |         __/ |
--                |_|        |___/

newtype Empty = Empty
    { protocolMagic :: Init Byron
    } deriving (Show)

cmdEmpty :: Mod CommandFields Cmd
cmdEmpty = command "empty" $
    info (helper <*> fmap CmdEmpty subCmd) mempty
  where
    subCmd = Empty <$> protocolMagicArg

protocolMagicArg :: Parser (Init Byron)
protocolMagicArg = fmap mkInit $ argument auto $
    metavar "PROTOCOL MAGIC"

--            _     _        _                   _
--           | |   | |      (_)                 | |
--   __ _  __| | __| |______ _ _ __  _ __  _   _| |_
--  / _` |/ _` |/ _` |______| | '_ \| '_ \| | | | __|
-- | (_| | (_| | (_| |      | | | | | |_) | |_| | |_
--  \__,_|\__,_|\__,_|      |_|_| |_| .__/ \__,_|\__|
--                                  | |
--                                  |_|

data AddInput = AddInput
    { inputIx :: Word32
    , inputTxId :: ByteString
    } deriving (Show)

cmdAddInput :: Mod CommandFields Cmd
cmdAddInput = command "add-input" $
    info (helper <*> fmap CmdAddInput subCmd) $ mconcat
        [ progDesc "Add a new input to the transaction."
        , headerDoc (Just $ vsep
            [ string "An input is made of:"
            , indent 2 $ string "- A input index."
            , indent 2 $ string "- An transaction id, 32 bytes, base16-encoded."
            ])
        , footerDoc (Just $ vsep
            [ string "Example:"
            , indent 2 $ string "add-input 0 3b40265...aad1c0b7"
            , indent 2 $ string "            <--- 64 CHARS --->"
            ])
        ]
  where
    subCmd = AddInput <$> inputIxArg <*> inputTxIdArg

inputIxArg :: Parser Word32
inputIxArg = argument auto $
    metavar "INDEX"

inputTxIdArg :: Parser ByteString
inputTxIdArg = bytesArgument (Just 32) fromBase16 $
    metavar "TXID"

--            _     _                   _               _
--           | |   | |                 | |             | |
--   __ _  __| | __| |______ ___  _   _| |_ _ __  _   _| |_
--  / _` |/ _` |/ _` |______/ _ \| | | | __| '_ \| | | | __|
-- | (_| | (_| | (_| |     | (_) | |_| | |_| |_) | |_| | |_
--  \__,_|\__,_|\__,_|      \___/ \__,_|\__| .__/ \__,_|\__|
--                                         | |
--                                         |_|

data AddOutput = AddOutput
    { coin :: Natural
    , address :: ByteString
    } deriving (Show)

cmdAddOutput :: Mod CommandFields Cmd
cmdAddOutput = command "add-output" $
    info (helper <*> fmap CmdAddOutput subCmd) $ mconcat
        [ progDesc "Add a new output to the transaction."
        , headerDoc (Just $ vsep
            [ string "An output is made of:"
            , indent 2 $ string "- A coin value in Lovelace (1 Ada = 1e6 Lovelace)."
            , indent 2 $ string "- A target address, base58-encoded."
            ])
        , footerDoc (Just $ vsep
            [ string "Example:"
            , indent 2 $ string "add-output 1000000 2cWKMJemo...ZTBMqHTPTkv"
            ])
        ]
  where
    subCmd = AddOutput <$> coinArg <*> addressArg

coinArg :: Parser Natural
coinArg = argument auto $
    metavar "LOVELACE"

addressArg :: Parser ByteString
addressArg = bytesArgument Nothing fromBase58 $
    metavar "ADDRESS"

--  _            _
-- | |          | |
-- | | ___   ___| | __
-- | |/ _ \ / __| |/ /
-- | | (_) | (__|   <
-- |_|\___/ \___|_|\_\

cmdLock :: Mod CommandFields Cmd
cmdLock = command "lock" $
    info (helper <*> pure CmdLock) $ mconcat
        [ progDesc "Lock the transaction and start signing inputs."
        , header
            "Once locked, it is no longer possible to add inputs or outputs to \
            \a transaction. This is a necessary step to be able to sign the \
            \transaction with private keys corresponding to inputs."
        ]

--      _             _    _ _ _   _
--     (_)           | |  | (_) | | |
--  ___ _  __ _ _ __ | |  | |_| |_| |__
-- / __| |/ _` | '_ \| |/\| | | __| '_ \
-- \__ \ | (_| | | | \  /\  / | |_| | | |
-- |___/_|\__, |_| |_|\/  \/|_|\__|_| |_|
--         __/ |
--        |___/

newtype SignWith = SignWith
    { prvKey :: ByteString
    } deriving (Show)

cmdSignWith :: Mod CommandFields Cmd
cmdSignWith = command "sign-with" $
    info (helper <*> fmap CmdSignWith subCmd) $ mconcat
        [ progDesc "Add a signature."
        ]
  where
    subCmd = SignWith <$> prvKeyArg

prvKeyArg :: Parser ByteString
prvKeyArg = bytesArgument (Just 96) fromBase16 $
    metavar "XPRV"

--                _       _ _
--               (_)     | (_)
--  ___  ___ _ __ _  __ _| |_ _______
-- / __|/ _ \ '__| |/ _` | | |_  / _ \
-- \__ \  __/ |  | | (_| | | |/ /  __/
-- |___/\___|_|  |_|\__,_|_|_/___\___|
--

cmdSerialize :: Mod CommandFields Cmd
cmdSerialize = command "serialize" $
    info (helper <*> pure CmdSerialize) $ mconcat
        [ progDesc "Serialize the signed transaction to binary."
        ]

--  _   _      _
-- | | | |    | |
-- | |_| | ___| |_ __   ___ _ __ ___
-- |  _  |/ _ \ | '_ \ / _ \ '__/ __|
-- | | | |  __/ | |_) |  __/ |  \__ \
-- \_| |_/\___|_| .__/ \___|_|  |___/
--              | |
--              |_|

-- | Parse a encoded 'Bytestring' argument.
bytesArgument
    :: Maybe Int -- ^ Number of bytes that are expected, if known.
    -> (Text -> Maybe ByteString) -- ^ A base conversion
    -> Mod ArgumentFields ByteString
    -> Parser ByteString
bytesArgument len fromBase =
    argument (maybeReader readerT)
  where
    readerT :: String -> Maybe ByteString
    readerT str = do
        bytes <- fromBase $ T.pack str
        guard (isNothing len || Just (BS.length bytes) == len)
        pure bytes

-- | Convert a 'ByteString' to 'Base64'
base64 :: ByteString -> Text
base64 = T.decodeUtf8 . convertToBase Base64

-- | Convert a /Base64/ 'Text' into a 'ByteString'
fromBase64 :: Text -> Maybe ByteString
fromBase64 = either (const Nothing) Just . convertFromBase Base64 . T.encodeUtf8

-- | Deserialize data from a /Base64/ CBOR 'ByteString', or fail.
hGetState :: Handle -> (forall s. CBOR.Decoder s a) -> IO a
hGetState h decoder = do
    bytes <- fromBase64 . T.decodeUtf8 <$> BS.hGetContents h
    case bytes of
        Nothing -> failWith
            "Unable to decode intermediate buffer. Did you manually crafted one?"
        Just cbor -> case CBOR.deserialiseFromBytes decoder (BL.fromStrict cbor) of
            Left e -> failWith (show e)
            Right (_, a) -> pure a

-- | Helper to output a given state to the console
hPutState :: Handle -> CBOR.Encoding -> IO ()
hPutState h = TIO.hPutStr h . base64 . CBOR.toStrictByteString

-- | Fail with a colored red error message.
failWith :: String -> IO a
failWith msg = do
    withSGR stderr (SetColor Foreground Vivid Red) $ do
        TIO.hPutStrLn stderr (T.pack msg)
    exitFailure

-- | Bracket-style constructor for applying ANSI Select Graphic Rendition to an
-- action and revert back to normal after.
--
-- This does nothing if the device isn't an ANSI terminal.
withSGR :: Handle -> SGR -> IO a -> IO a
withSGR h sgr action = hIsTerminalDevice h >>= \case
    True  -> bracket aFirst aLast aBetween
    False -> action
  where
    aFirst = ([] <$ hSetSGR h [sgr])
    aLast = hSetSGR h
    aBetween = const action
