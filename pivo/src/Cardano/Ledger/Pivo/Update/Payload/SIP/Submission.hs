{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Pivo.Update.Payload.SIP.Submission where

import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Control.DeepSeq (NFData ())
import NoThunks.Class (NoThunks ())
import Data.Aeson (ToJSON, FromJSON)
import Data.Set (Set, singleton)
import Data.Text (Text)

import Cardano.Crypto.DSIGN (hashVerKeyDSIGN)
import qualified Cardano.Crypto.Hash as Cardano
import Cardano.Binary (ToCBOR (toCBOR), FromCBOR (fromCBOR), encodeListLen, decodeListLenOf)

import qualified Shelley.Spec.Ledger.Keys as Shelley

import           Cardano.Ledger.Era (Era)
import qualified Cardano.Ledger.Era as Era

import Cardano.Ledger.Pivo.Update.Payload.SIP.Proposal (Proposal, mkProposal)
import Cardano.Ledger.Pivo.Update.Payload.Types (Hash, VKeyHash, VKey)

data Submission era =
  Submission
  { author :: VKeyHash era
  -- ^ Submission author. This will be compared against the revelator key in
  -- 'Revelation'.
  , commit :: Commit era
  } deriving (Eq, Show, Generic, NFData, NoThunks, ToJSON)

type Commit era = Hash era (Int, VKeyHash era, Proposal era)

instance (Typeable era, Era era) => ToCBOR (Submission era) where
  toCBOR Submission { author, commit }
    =  encodeListLen 2
    <> toCBOR author
    <> toCBOR commit

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
witnesses
  :: Submission era -> Set (Shelley.KeyHash 'Shelley.Witness (Era.Crypto era))
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
    { author = vkHash
    , commit = mkCommit someSalt vkHash proposal
    }
    where
      vkHash   = hashVerKeyDSIGN vk
      proposal = mkProposal someText

mkCommit
  :: Era era
  => Int
  -> VKeyHash era
  -> Proposal era
  -> Commit era
mkCommit someSalt vkHash proposal =
  Cardano.hashWithSerialiser toCBOR (someSalt, vkHash, proposal)
