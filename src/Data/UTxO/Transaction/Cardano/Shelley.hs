{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_HADDOCK prune #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Data.UTxO.Transaction.Cardano.Shelley
    (
    -- * Initialization
      mkInit

    -- * Types
    , NetworkId (..)
    , NetworkMagic (..)
    , Fee (..)
    , AddrAttributes

   -- * Constructing Primitives
    , mkInput
    , mkOutput
    , mkShelleySignKey
    , mkByronSignKey
    , mkAddrAttributes

    -- Internal
    , Shelley
    ) where


import Cardano.Address.Byron
    ( AddrAttributes, Address (..), getAddrAttributes, mkAttributes )
import Cardano.Api.Typed
    ( NetworkId, NetworkMagic (..), TxIn (..), TxOut (..) )
import Cardano.Crypto.Extra
    ( xprvFromBytes )
import Cardano.Crypto.Hash.Class
    ( Hash (UnsafeHash) )
import Cardano.Crypto.Signing
    ( SigningKey (..) )
import Cardano.Slotting.Slot
    ( SlotNo (..) )
import Data.ByteString
    ( ByteString )
import Data.ByteString.Short
    ( toShort )
import Data.Foldable
    ( asum )
import Data.UTxO.Transaction
    ( ErrMkPayment (..), MkPayment (..) )
import Data.Word
    ( Word32 )
import Numeric.Natural
    ( Natural )

import qualified Cardano.Api.Typed as Cardano
import qualified Data.ByteString as BS
import qualified Shelley.Spec.Ledger.Address.Bootstrap as Ledger
import qualified Shelley.Spec.Ledger.TxBody as Ledger


-- | A type isomorphic to 'Integer' to represent fees.
newtype Fee = Fee { unFee :: Integer }
    deriving (Show, Eq, Num)

-- | A type to capture signing keys in Shelley. In order to produce Byron
-- witnesses, one needs to include extra information that are present in the
-- source input address.
data CardanoSigningKey
    = ShelleySigningKey Cardano.ShelleyWitnessSigningKey
    | ByronSigningKey AddrAttributes SigningKey

-- | Construct a payment 'Init' for /Shelley/ from primitive types.
--
-- __examples__:
--
-- >>> mkInit Mainnet 430000
-- >>> mkInit (Testnet (NetworkMagic 1234)) 430000
--
-- @since 2.0.0
mkInit
    :: NetworkId
        -- ^ A network tag, Mainnet or Testnet with NetworkMagic specified
    -> SlotNo
        -- ^ A ttl expressed in slot number counted from the beginning of blockchain
    -> Fee
        -- ^ fee of tx as taken when constructing change outputs
    -> Init Shelley
mkInit net ttl fee = (net, ttl, fee)

-- | Construct a payment 'Input' for /Shelley/ from primitive types.
--
-- __example__:
--
-- >>> mkInput 14 =<< fromBase16 "3b402651...aad1c0b7"
-- Just (Input ...)
--
-- @since 2.0.0
mkInput
    :: Word32
        -- ^ Input index.
    -> ByteString
        -- ^ Input transaction id. See also: 'fromBase16'.
    -> Maybe (Input Shelley)
mkInput ix bytes =
    if BS.length bytes == 32 then
        Just $ Cardano.TxIn
            (Cardano.TxId $ UnsafeHash $ toShort bytes)
            (Cardano.TxIx (fromIntegral ix))
    else
        Nothing

-- | Construct a payment 'Output' for /Shelley/ from primitive types.
--
-- __example__:
--
-- >>> mkOutput 42 =<< fromBase58 "Ae2tdPwU...DnXy319f"
-- >>> mkOutput 42 =<< fromBech32 "addr1sjc...6s3xvu5g"
-- >>> mkOutput 42 =<< fromBase16 "42bf330c...ba5b947e"
-- Just (Output ...)
--
-- @since 2.0.0
mkOutput
    :: Natural
        -- ^ Output value, in Lovelace (1 Ada = 1e6 Lovelace).
    -> ByteString
        -- ^ Output Address. See also: 'fromBase58', 'fromBase16', 'fromBech32'.
    -> Maybe (Output Shelley)
mkOutput coin bytes =
    (\addr -> Cardano.TxOut addr (toLovelace coin)) <$> asum
    [ Cardano.AddressInEra (Cardano.ShelleyAddressInEra Cardano.ShelleyBasedEraShelley)
        <$> Cardano.deserialiseFromRawBytes Cardano.AsShelleyAddress bytes

    , Cardano.AddressInEra Cardano.ByronAddressInAnyEra
        <$> Cardano.deserialiseFromRawBytes Cardano.AsByronAddress bytes
    ]
  where
    toLovelace
        = Cardano.TxOutAdaOnly Cardano.AdaOnlyInShelleyEra
        . Cardano.Lovelace
        . fromIntegral

-- | Construct a 'SignKey' for /Shelley/ from primitive types.
-- This is for Shelley era keys.
--
-- __example__:
--
-- >>> mkShelleySignKey =<< fromBech32 "xprv13f0ve...nu4v4h875l"
-- Just (SignKey ...)
--
-- @since 2.0.0
mkShelleySignKey
    :: ByteString
        -- ^ A extended address private key and its chain code.
        -- The key __must be 96 bytes__ long, internally made of two concatenated parts:
        --
        -- @
        -- BYTES = PRV | CC
        -- PRV   = 64OCTET  # a 64 bytes Ed25519 extended private key
        -- CC    = 32OCTET  # a 32 bytes chain code
        -- @
        --
        -- See also: 'fromBech32'.
    -> Maybe (SignKey Shelley)
mkShelleySignKey = fmap
    ( ShelleySigningKey
    . Cardano.WitnessPaymentExtendedKey
    . Cardano.PaymentExtendedSigningKey
    ) . xprvFromBytes

-- | Construct a 'SignKey' for /Shelley/ from primitive types.
-- This is for Byron era keys.
--
-- __example__:
--
-- >>> let (Just addr) = fromBase58 "DdzFFzCqrh...Dwg3SiaHiEL"
-- >>> mkByronSignKey addr =<< fromBech32 "xprv13f0ve...nu4v4h875l"
-- Just (SignKey ...)
--
-- @since 2.0.0
mkByronSignKey
    :: AddrAttributes
        -- ^ Address attributes, obtained from a Byron address.
        -- See also: 'mkAddrAttributes'
    -> ByteString
        -- ^ A extended address private key and its chain code.
        -- The key __must be 96 bytes__ long, internally made of two concatenated parts:
        --
        -- @
        -- BYTES = PRV | CC
        -- PRV   = 64OCTET  # a 64 bytes Ed25519 extended private key
        -- CC    = 32OCTET  # a 32 bytes chain code
        -- @
        --
        -- See also: 'fromBase16'.
    -> Maybe (SignKey Shelley)
mkByronSignKey addrAttrs = do
    fmap (ByronSigningKey addrAttrs . SigningKey) . xprvFromBytes

-- | Extract address attributes from an address, if they exists (i.e. if the
-- address is a bootstrap / Byron address).
--
-- __example__:
--
-- >>> let (Just addr) = fromBase58 "DdzFFzCqrh...Dwg3SiaHiEL"
-- >>> mkAddrAttributes addr
-- Just (AddrAttributes ...)
--
-- @since 2.0.0
mkAddrAttributes
    :: ByteString
        -- ^ A Byron address, as a raw 'ByteString'.
    -> Maybe AddrAttributes
mkAddrAttributes =
    getAddrAttributes . Address

--
-- MkPayment instance
--

-- Type-level constructor capturing types for 'Shelley'.
data Shelley

instance MkPayment Shelley where
    type Init Shelley = (NetworkId, SlotNo, Fee)

    type Input   Shelley = TxIn
    type Output  Shelley = TxOut Cardano.Shelley
    type SignKey Shelley = CardanoSigningKey

    type CoinSel Shelley =
        (NetworkId, SlotNo, Fee, [TxIn], [TxOut Cardano.Shelley])

    type Tx Shelley = Either
        ErrMkPayment
        ( NetworkId
        , [TxIn]
        , [TxOut Cardano.Shelley]
        , Cardano.TxBody Cardano.Shelley
        , [Cardano.Witness Cardano.Shelley]
        )

    empty :: Init Shelley -> CoinSel Shelley
    empty (net, ttl, fee) = (net, ttl, fee, mempty, mempty)

    addInput :: TxIn -> CoinSel Shelley -> CoinSel Shelley
    addInput inp (pm, ttl, fee, inps, outs) = (pm, ttl, fee, inp : inps, outs)

    addOutput :: TxOut Cardano.Shelley -> CoinSel Shelley -> CoinSel Shelley
    addOutput out (pm, ttl, fee, inps, outs) = (pm, ttl, fee, inps, out : outs)

    lock :: CoinSel Shelley -> Tx Shelley
    lock (_net, _ttl, _fee, [], _outs) = Left MissingInput
    lock (_net, _ttl, _fee, _inps, []) = Left MissingOutput
    lock (net, ttl, fee, inps, outs) = Right (net, inps', outs', sigData, mempty)
      where
        inps'   = reverse inps
        outs'   = reverse outs
        Right sigData = Cardano.makeShelleyTransaction
            inps'
            outs'
            ttl
            (Cardano.Lovelace $ unFee fee)
            [] -- certificates
            [] -- withdrawals
            Nothing -- metadata
            Nothing -- update proposals

    signWith :: SignKey Shelley -> Tx Shelley -> Tx Shelley
    signWith _ (Left e) = Left e

    signWith (ShelleySigningKey skey) (Right (net, inps, outs, sigData, wits)) =
        Right (net, inps, outs, sigData, shelleyWit : wits)
      where
        shelleyWit = Cardano.makeShelleyKeyWitness sigData skey

    signWith (ByronSigningKey addrAttrs skey) (Right (net, inps, outs, sigData, wits)) =
        Right (net, inps, outs, sigData, byronWit : wits)
      where
        byronWit = Cardano.ShelleyBootstrapWitness era $
            Ledger.makeBootstrapWitness txHash skey attrs
        Cardano.ShelleyTxBody era body _ = sigData
        txHash = Ledger.eraIndTxBodyHash body
        attrs  = mkAttributes addrAttrs

    serialize :: Tx Shelley -> Either ErrMkPayment ByteString
    serialize (Left e) = Left e
    serialize (Right (_net, inps, _outs, sigData, wits))
        | length inps /= length wits = Left MissingSignature
        | otherwise = Right $ Cardano.serialiseToCBOR tx
      where
        tx = Cardano.makeSignedTransaction wits sigData
