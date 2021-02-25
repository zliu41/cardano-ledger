{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Pivo.Update.Payload.SIP where

import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Control.DeepSeq (NFData ())
import NoThunks.Class (NoThunks ())
import Data.Aeson (ToJSON, FromJSON)
import Data.Set (Set, singleton)
import Data.Text (Text)

import Cardano.Crypto.DSIGN (VerKeyDSIGN, hashVerKeyDSIGN)
import qualified Cardano.Crypto.Hash as Cardano (Hash, hashWithSerialiser)
import Cardano.Binary (ToCBOR (toCBOR), FromCBOR (fromCBOR), encodeListLen, decodeListLenOf)

import qualified Shelley.Spec.Ledger.Keys as Shelley

import Cardano.Ledger.Crypto (HASH, ADDRHASH, DSIGN)
import           Cardano.Ledger.Era (Era)
import qualified Cardano.Ledger.Era as Era

--------------------------------------------------------------------------------
-- Common types used throughout the module
--------------------------------------------------------------------------------

type VKey era = VerKeyDSIGN (DSIGN (Era.Crypto era))

type Hash era a = Cardano.Hash (HASH (Era.Crypto era)) a

type VKeyHash era = Cardano.Hash (ADDRHASH (Era.Crypto era)) (VKey era)

--------------------------------------------------------------------------------
-- Submission
--------------------------------------------------------------------------------

data Submission era =
  Submission
  { author :: VKeyHash era
  -- ^ Submission author. This will be compared against the revelator key in
  -- 'Revelation'.
  , commitHash :: Hash era (Int, VKeyHash era, Proposal era)
  } deriving (Eq, Show, Generic, NFData, NoThunks, ToJSON)

instance (Typeable era, Era era) => ToCBOR (Submission era) where
  toCBOR Submission { author, commitHash }
    =  encodeListLen 2
    <> toCBOR author
    <> toCBOR commitHash

instance (Typeable era, Era era) => FromCBOR (Submission era) where
  fromCBOR = do
    decodeListLenOf 2
    a <- fromCBOR
    h <- fromCBOR
    return $! Submission a h

deriving instance Era era => FromJSON (Submission era)

-- | Extract the commit witness.
--
-- TODO: we might want to put this function inside a typeclass.
witnesses :: Submission era -> Set (Shelley.KeyHash 'Shelley.Witness (Era.Crypto era))
witnesses = singleton . Shelley.KeyHash . author

mkSubmission
  :: Era era
  => VKey era
  -> Int
  -- ^ Salt used to calculate the submission commit.
  -> Text
  -- ^ Proposal's text.
  -> Submission era
mkSubmission vk someSalt someText =
  Submission
    { author     = vkHash
    , commitHash =
        Cardano.hashWithSerialiser toCBOR (someSalt, vkHash, proposal)
    }
    where
      vkHash   = hashVerKeyDSIGN vk
      proposal = Proposal (Cardano.hashWithSerialiser toCBOR someText)


--------------------------------------------------------------------------------
-- Revelation
--------------------------------------------------------------------------------

data Revelation era
  = Revelation
    { proposal  :: Proposal era
      -- ^ Proposal that is being revealed.
    , revelator :: VKeyHash era
    , salt      :: Int
      -- ^ Salt used to calculate the commit.
    }

data Proposal era =
  Proposal { dataHash :: Hash era Text
             -- ^ Hash of the proposal's data. For now we assume the proposal is
             -- simply a string.
           }
  deriving (Eq, Show, Generic, NFData, NoThunks, ToJSON)

instance (Typeable era, Era era) => ToCBOR (Proposal era) where
  toCBOR (Proposal { dataHash }) =
    encodeListLen 1 <> toCBOR dataHash

instance (Typeable era, Era era) => FromCBOR (Proposal era) where
  fromCBOR = do
    decodeListLenOf 1
    h <- fromCBOR
    return $! Proposal h

deriving instance Era era => FromJSON (Proposal era)
