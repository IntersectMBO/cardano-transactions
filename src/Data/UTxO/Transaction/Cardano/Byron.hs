{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

module Data.UTxO.Transaction.Cardano.Byron
    (
    -- * Type
    Byron

    -- * Helpers
    , mkOutput'
    , decodeAddressBase58
    , encodeAddressBase58
    ) where

import Codec.CBOR.Read
    ( deserialiseFromBytes )
import Data.ByteString
    ( ByteString )
import Data.List.NonEmpty
    ( NonEmpty )
import Data.UTxO.Transaction
    ( N (..), UTxOTransaction (..) )
import Data.Word
    ( Word32 )
import GHC.Exts
    ( IsList (fromList) )
import Numeric.Natural
    ( Natural )

import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteString.Lazy as BL
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Encoding as T

import Cardano.Binary
    ( FromCBOR (..), ToCBOR (..) )
import Cardano.Chain.Common
    ( Address
    , decodeAddressBase58
    , encodeAddressBase58
    , mkAttributes
    , mkLovelace
    )
import Cardano.Chain.UTxO
    ( TxAux (..)
    , TxIn (..)
    , TxInWitness (..)
    , TxOut (..)
    , TxSigData (..)
    , mkTxAux
    )
import Cardano.Crypto.Hashing
    ( decodeHash, hash )
import Cardano.Crypto.ProtocolMagic
    ( ProtocolMagicId (..) )
import Cardano.Crypto.Signing
    ( SignTag (..), Signature, SigningKey (..), VerificationKey (..) )
import Cardano.Crypto.Wallet
    ( toXPub, xprv )

import qualified Cardano.Chain.UTxO as CC
import qualified Cardano.Crypto.Signing as CC


data Byron

-- | Internal type use for constructing a coin selection
data ByronCoinSel (nInp :: N) (nOut :: N) = ByronCoinSel
    { csProtocolMagicId :: ProtocolMagicId
    , csInputs :: [TxIn]
    , csOutputs :: [TxOut]
    }

-- | Internal type use for constructing a transaction
data ByronTx (nWit :: N) = ByronTx
    { txProtocolMagicId :: ProtocolMagicId
    , txInputs :: NonEmpty TxIn
    , txOutputs :: NonEmpty TxOut
    , txSigData :: TxSigData
    , txWits :: [TxInWitness]
    }

instance UTxOTransaction Byron where
    type Init Byron = ProtocolMagicId

    type Input   Byron = TxIn
    type Output  Byron = TxOut
    type SignKey Byron = SigningKey

    type CoinSel Byron = ByronCoinSel
    type Tx      Byron = ByronTx

    mkInput :: ByteString -> Word32 -> Maybe TxIn
    mkInput bytes ix =
        case decodeHash (T.decodeUtf8 bytes) of
            Right txId -> Just $ TxInUtxo txId ix
            Left{} -> Nothing

    mkOutput :: ByteString -> Natural -> Maybe TxOut
    mkOutput bytes n =
        case (fromCBOR' bytes, mkLovelace (fromIntegral n)) of
            (Right addr, Right coin) -> Just $ TxOut addr coin
            _ -> Nothing
      where
        fromCBOR' = fmap snd . deserialiseFromBytes fromCBOR .  BL.fromStrict

    -- FIXME
    -- 'xprv' assumes that the bytes represent the concatenation of
    --
    -- - An extended private key (32 bytes)
    -- - An extended public key (64 bytes)
    -- - A chain code (32 bytes)
    --
    -- whereas here, we probably really want the bytes to be only representing
    --
    -- - An extended private key (32 bytes)
    -- - A chain code (32 bytes)
    --
    -- The extended public key can and should be computed from the extended
    -- public key.
    mkSignKey :: ByteString -> Maybe SigningKey
    mkSignKey bytes = case xprv bytes of
        Left{} -> Nothing
        Right prv -> Just $ SigningKey prv

    empty :: ProtocolMagicId -> CoinSel Byron Zero Zero
    empty pm = ByronCoinSel pm mempty mempty

    addInput :: TxIn -> CoinSel Byron n m -> CoinSel Byron (Some n) m
    addInput inp cs = cs { csInputs = inp : csInputs cs }

    addOutput :: TxOut -> CoinSel Byron n m -> CoinSel Byron n (Some m)
    addOutput out cs = cs { csOutputs = out : csOutputs cs }

    lock :: CoinSel Byron (Some n) (Some m) -> Tx Byron (Some n)
    lock cs = ByronTx
        { txProtocolMagicId = csProtocolMagicId cs
        , txInputs  = neInps
        , txOutputs = neOuts
        , txSigData = TxSigData $ hash $ CC.UnsafeTx neInps neOuts (mkAttributes ())
        , txWits = mempty
        }
      where
        -- Safe because inputs are guaranteed to be non-empty by construction
        neInps = NE.fromList $ reverse $ csInputs cs
        -- Safe because ouputs are guaranteed to be non-empty by construction
        neOuts = NE.fromList $ reverse $ csOutputs cs

    addWitness :: SigningKey -> Tx Byron (Some n) -> Tx Byron n
    addWitness (SigningKey prv) tx =
        tx { txWits = VKWitness vk sig : txWits tx }
      where
        vk :: VerificationKey
        vk = VerificationKey (toXPub prv)

        pm :: ProtocolMagicId
        pm = txProtocolMagicId tx

        sig :: Signature TxSigData
        sig = CC.sign pm SignTx (SigningKey prv) (txSigData tx)

    serialize :: Tx Byron Zero -> ByteString
    serialize tx = CBOR.toStrictByteString $ toCBOR $ mkTxAux
        (CC.UnsafeTx (txInputs tx) (txOutputs tx) (mkAttributes ()))
        (fromList $ reverse $ txWits tx)

-- | An alternative to 'mkOutput' which works directly from 'Address'
mkOutput' :: Address -> Natural -> Maybe TxOut
mkOutput' addr n =
    case mkLovelace (fromIntegral n) of
        Left{} -> Nothing
        Right coin -> Just $ TxOut addr coin
