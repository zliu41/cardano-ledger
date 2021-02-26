{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Pivo.Update
  ( Payload ( Payload
            , sipSubmissions
            , sipRevelations
            )
  , witnesses
  , Environment (Environment)
  , State
  , PredicateFailure (NoFailure) -- It's important to expose this so that other
                                 -- modules can define a "ToObject" instance.
  )
where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData ())
import NoThunks.Class (NoThunks ())
import Data.Typeable (Typeable)
import qualified Data.Text as Text
import Data.Set (Set)
import Data.Default.Class (Default, def)
import Data.Sequence.Strict (StrictSeq)

import Data.Aeson (ToJSON, FromJSON)

import Cardano.Prelude (cborError)
import Cardano.Binary (ToCBOR (toCBOR), encodeWord, FromCBOR (fromCBOR), decodeWord
                      , encodeListLen, decodeListLenOf)
import Data.Coders (encodeFoldable, decodeStrictSeq)

import Cardano.Ledger.Era (Crypto, Era)

import Shelley.Spec.Ledger.Keys (KeyHash, KeyRole (Witness))

import qualified Cardano.Ledger.Pivo.Update.Payload.SIP as SIP

import Shelley.Spec.Ledger.TxBody ()

data Payload era =
  Payload { sipSubmissions :: !(StrictSeq (SIP.Submission era))
          , sipRevelations :: !(StrictSeq (SIP.Revelation era))
          }
  deriving (Show, Eq, NFData, NoThunks, Generic, ToJSON, FromJSON)

instance (Typeable era, Era era) => ToCBOR (Payload era) where
  toCBOR Payload { sipSubmissions, sipRevelations }
    =  encodeListLen 2
    <> encodeFoldable sipSubmissions
    <> encodeFoldable sipRevelations

instance (Typeable era, Era era) => FromCBOR (Payload era) where
  fromCBOR = do
    decodeListLenOf 2
    sipSubs <- decodeStrictSeq fromCBOR
    sipRevs <- decodeStrictSeq fromCBOR
    return $! Payload sipSubs sipRevs

-- | Key hashes that have to witness the update payload.
witnesses :: Payload era -> Set (KeyHash 'Witness (Crypto era))
witnesses = foldMap SIP.witnesses. sipSubmissions

--------------------------------------------------------------------------------
-- Update environment
--------------------------------------------------------------------------------

data Environment era = Environment
  deriving (Show, NFData, Generic, Eq, NoThunks, ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Update state
--------------------------------------------------------------------------------

-- | Update state. This is shared among all the update rules (e.g. PUP and UPEC)
data State era = State
  deriving (Show, NFData, Generic, Eq, NoThunks, ToJSON, FromJSON)

instance Default (State era) where
  def = State

instance Typeable era => ToCBOR (State era) where
  toCBOR State = encodeWord 0

instance Typeable era => FromCBOR (State era) where
  fromCBOR = decodeWord >>= \case
      0 -> pure State
      k -> cborError $  "Invalid key " <> (Text.pack (show k))
                     <> " when decoding a value of type State"

--------------------------------------------------------------------------------
-- Predicate failure
--------------------------------------------------------------------------------

data PredicateFailure era = NoFailure
  deriving (Show, NFData, Generic, Eq, NoThunks)

instance Typeable era => ToCBOR (PredicateFailure era) where
  toCBOR NoFailure = encodeWord 0

instance Typeable era => FromCBOR (PredicateFailure era) where
  fromCBOR = decodeWord >>= \case
      0 -> pure NoFailure
      k -> cborError $  "Invalid key " <> (Text.pack (show k))
                     <> " when decoding a value of type State"
