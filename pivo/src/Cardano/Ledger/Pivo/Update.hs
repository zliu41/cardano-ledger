{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveAnyClass #-}

module Cardano.Ledger.Pivo.Update
  ( Payload (Payload)
  , witnesses
  , Environment (Environment)
  , State
  , PredicateFailure (NoFailure) -- It's important to expose this to that other
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

import Cardano.Prelude (cborError)
import Cardano.Binary (ToCBOR (toCBOR), encodeWord, FromCBOR (fromCBOR), decodeWord)

import Cardano.Ledger.Era (Crypto)

import Shelley.Spec.Ledger.Keys (KeyHash, KeyRole (Witness))

data Payload era = Payload
  deriving (Show, NFData, Generic, Eq, NoThunks)

instance Typeable era => ToCBOR (Payload era) where
  toCBOR Payload = encodeWord 0

instance Typeable era => FromCBOR (Payload era) where
  fromCBOR = decodeWord >>= \case
      0 -> pure Payload
      k -> cborError $  "Invalid key " <> (Text.pack (show k))
                     <> " when decoding a value of type Payload"

-- | Key hashes that have to witness the update payload.
witnesses :: Payload era -> Set (KeyHash 'Witness (Crypto era))
witnesses = mempty

--------------------------------------------------------------------------------
-- Update environment
--------------------------------------------------------------------------------

data Environment era = Environment
  deriving (Show, NFData, Generic, Eq, NoThunks)

--------------------------------------------------------------------------------
-- Update state
--------------------------------------------------------------------------------

-- | Update state. This is shared among all the update rules (e.g. PUP and UPEC)
data State era = State
  deriving (Show, NFData, Generic, Eq, NoThunks)

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
