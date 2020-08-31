{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Shelley.Spec.Ledger.TxDataShelley
  ( TxBody
      ( TxBody,
        _inputs,
        _outputs,
        _certs,
        _wdrls,
        _txfee,
        _ttl,
        _txUpdate,
        _mdHash,
        extraSize
      ),
  )
where

import Cardano.Binary
  ( Annotator (..),
    Case (..),
    Decoder,
    DecoderError (..),
    FromCBOR (fromCBOR),
    Size,
    ToCBOR (..),
    annotatorSlice,
    decodeWord,
    encodeListLen,
    encodeMapLen,
    encodePreEncoded,
    encodeWord,
    serializeEncoding,
    serializeEncoding',
    szCases,
    withSlice,
  )
import Cardano.Ledger.Era
import Cardano.Prelude
  ( AllowThunksIn (..),
    LByteString,
    NFData (rnf),
    NoUnexpectedThunks (..),
    UseIsNormalFormNamed (..),
    Word64,
    asum,
    catMaybes,
    cborError,
    panic,
  )
import Control.Iterate.SetAlgebra (BaseRep (MapR), Embed (..), Exp (Base), HasExp (toExp))
import Control.Monad (unless)
import Data.Aeson (FromJSON (..), ToJSON (..), (.!=), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (explicitParseField)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as BSS
import Data.Foldable (fold)
import Data.IP (IPv4, IPv6)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import Data.Proxy (Proxy (..))
import Data.Relation (Relation (..))
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text.Encoding as Text
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Quiet
import Shelley.Spec.Ledger.Address
  ( Addr (..),
    RewardAcnt (..),
    deserialiseAddr,
    serialiseAddr,
  )
import Shelley.Spec.Ledger.BaseTypes
  ( DnsName,
    Port,
    StrictMaybe (..),
    UnitInterval,
    Url,
    invalidKey,
    maybeToStrictMaybe,
    strictMaybeToMaybe,
  )
import Shelley.Spec.Ledger.Coin (Coin (..), word64ToCoin)
import Shelley.Spec.Ledger.Credential
  ( Credential (..),
    Ix,
    Ptr (..),
    StakeCredential,
  )
import Shelley.Spec.Ledger.DeserializeShort (deserializeShortAddr)
import Shelley.Spec.Ledger.Hashing
import Shelley.Spec.Ledger.Keys
  ( Hash,
    KeyHash (..),
    KeyRole (..),
    SignedDSIGN,
    VKey,
    VerKeyVRF,
    asWitness,
    decodeSignedDSIGN,
    encodeSignedDSIGN,
    hashKey,
  )
import Shelley.Spec.Ledger.MetaData (MetaDataHash)
import Shelley.Spec.Ledger.Orphans ()
import Shelley.Spec.Ledger.PParams (Update)
import Shelley.Spec.Ledger.Serialization
  ( CBORGroup (..),
    CborSeq (..),
    FromCBORGroup (..),
    ToCBORGroup (..),
    decodeMapContents,
    decodeNullMaybe,
    decodeRecordNamed,
    decodeRecordSum,
    decodeSet,
    decodeStrictSeq,
    encodeFoldable,
    encodeNullMaybe,
    ipv4FromCBOR,
    ipv4ToCBOR,
    ipv6FromCBOR,
    ipv6ToCBOR,
    listLenInt,
    mapFromCBOR,
    mapToCBOR,
  )
import Shelley.Spec.Ledger.Slot (EpochNo (..), SlotNo (..))

import Shelley.Spec.Ledger.TxDataCommon

-- | A raw transaction
data TxBody era = TxBody'
  { _inputs' :: !(Set (TxIn era)),
    _outputs' :: !(StrictSeq (TxOut era)),
    _certs' :: !(StrictSeq (DCert era)),
    _wdrls' :: !(Wdrl era),
    _txfee' :: !Coin,
    _ttl' :: !SlotNo,
    _txUpdate' :: !(StrictMaybe (Update era)),
    _mdHash' :: !(StrictMaybe (MetaDataHash era)),
    bodyBytes :: LByteString,
    extraSize :: !Int64 -- This is the contribution of inputs, outputs, and fees to the size of the transaction
  }
  deriving (Show, Eq, Generic)
  deriving
    (NoUnexpectedThunks)
    via AllowThunksIn '["bodyBytes"] (TxBody era)

instance Era era => HashAnnotated (TxBody era) era

pattern TxBody ::
  Era era =>
  Set (TxIn era) ->
  StrictSeq (TxOut era) ->
  StrictSeq (DCert era) ->
  Wdrl era ->
  Coin ->
  SlotNo ->
  StrictMaybe (Update era) ->
  StrictMaybe (MetaDataHash era) ->
  TxBody era
pattern TxBody {_inputs, _outputs, _certs, _wdrls, _txfee, _ttl, _txUpdate, _mdHash} <-
  TxBody'
    { _inputs' = _inputs,
      _outputs' = _outputs,
      _certs' = _certs,
      _wdrls' = _wdrls,
      _txfee' = _txfee,
      _ttl' = _ttl,
      _txUpdate' = _txUpdate,
      _mdHash' = _mdHash
    }
  where
    TxBody _inputs _outputs _certs _wdrls _txfee _ttl _txUpdate _mdHash =
      let encodeMapElement ix enc x = Just (encodeWord ix <> enc x)
          encodeMapElementUnless condition ix enc x =
            if condition x
              then Nothing
              else encodeMapElement ix enc x
          l =
            catMaybes
              [ encodeMapElement 0 encodePreEncoded inputBytes,
                encodeMapElement 1 encodePreEncoded outputBytes,
                encodeMapElement 2 encodePreEncoded feeBytes,
                encodeMapElement 3 toCBOR _ttl,
                encodeMapElementUnless null 4 encodeFoldable _certs,
                encodeMapElementUnless (null . unWdrl) 5 toCBOR _wdrls,
                encodeMapElement 6 toCBOR =<< strictMaybeToMaybe _txUpdate,
                encodeMapElement 7 toCBOR =<< strictMaybeToMaybe _mdHash
              ]
          inputBytes = serializeEncoding' $ encodeFoldable _inputs
          outputBytes = serializeEncoding' $ encodeFoldable _outputs
          feeBytes = serializeEncoding' $ toCBOR _txfee
          es =
            fromIntegral $
              BS.length inputBytes
                + BS.length outputBytes
                + BS.length feeBytes
          n = fromIntegral $ length l
          bytes = serializeEncoding $ encodeMapLen n <> fold l
       in TxBody'
            _inputs
            _outputs
            _certs
            _wdrls
            _txfee
            _ttl
            _txUpdate
            _mdHash
            bytes
            es

{-# COMPLETE TxBody #-}

-- CBOR

instance
  (Era era) =>
  ToCBOR (TxBody era)
  where
  toCBOR = encodePreEncoded . BSL.toStrict . bodyBytes

instance
  (Era era) =>
  FromCBOR (Annotator (TxBody era))
  where
  fromCBOR = annotatorSlice $ do
    mapParts <-
      decodeMapContents $
        decodeWord >>= \case
          0 -> f 0 (decodeSet fromCBOR) $ \bytes x t ->
            t
              { _inputs' = x,
                extraSize = extraSize t + BSL.length bytes
              }
          1 -> f 1 (decodeStrictSeq fromCBOR) $ \bytes x t ->
            t
              { _outputs' = x,
                extraSize = extraSize t + BSL.length bytes
              }
          2 -> f 2 fromCBOR $ \bytes x t ->
            t
              { _txfee' = x,
                extraSize = extraSize t + BSL.length bytes
              }
          3 -> f 3 fromCBOR $ \_ x t -> t {_ttl' = x}
          4 -> f 4 (decodeStrictSeq fromCBOR) $ \_ x t -> t {_certs' = x}
          5 -> f 5 fromCBOR $ \_ x t -> t {_wdrls' = x}
          6 -> f 6 fromCBOR $ \_ x t -> t {_txUpdate' = SJust x}
          7 -> f 7 fromCBOR $ \_ x t -> t {_mdHash' = SJust x}
          k -> invalidKey k
    let requiredFields :: Map Int String
        requiredFields =
          Map.fromList $
            [ (0, "inputs"),
              (1, "outputs"),
              (2, "fee"),
              (3, "ttl")
            ]
        fields = fst <$> mapParts
        missingFields = Map.filterWithKey (\k _ -> notElem k fields) requiredFields
    unless
      (null missingFields)
      (fail $ "missing required transaction component(s): " <> show missingFields)
    pure $
      Annotator $
        \fullbytes bytes ->
          (foldr ($) basebody (flip runAnnotator fullbytes . snd <$> mapParts)) {bodyBytes = bytes}
    where
      f ::
        Int ->
        Decoder s a ->
        (LByteString -> a -> TxBody era -> TxBody era) ->
        Decoder s (Int, Annotator (TxBody era -> TxBody era))
      f key decoder updater = do
        (x, annBytes) <- withSlice decoder
        let result = Annotator $ \fullbytes txbody ->
              updater (runAnnotator annBytes fullbytes) x txbody
        pure (key, result)
      basebody =
        TxBody'
          { _inputs' = Set.empty,
            _outputs' = StrictSeq.empty,
            _txfee' = Coin 0,
            _ttl' = SlotNo 0,
            _certs' = StrictSeq.empty,
            _wdrls' = Wdrl Map.empty,
            _txUpdate' = SNothing,
            _mdHash' = SNothing,
            bodyBytes = mempty,
            extraSize = 0
          }
