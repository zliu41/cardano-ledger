{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Pivo.Update.Payload.SIP.Proposal where

import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Control.DeepSeq (NFData ())
import NoThunks.Class (NoThunks ())
import Data.Aeson (ToJSON, FromJSON, ToJSONKey, FromJSONKey)
import Data.Text (Text)

import Cardano.Binary (ToCBOR (toCBOR), FromCBOR (fromCBOR), encodeListLen, decodeListLenOf)
import qualified Cardano.Crypto.Hash as Cardano
import           Cardano.Slotting.Slot (SlotNo)

import Cardano.Ledger.Update.Proposal (Identifiable (Id, _id), Signed (signatureVerifies))

import           Cardano.Ledger.Era (Era)

import Cardano.Ledger.Pivo.Update.Payload.Types (Hash)

data Proposal era =
  Proposal
    { proposalTextHash :: Hash era Text
      -- ^ Hash of the proposal's text. For now we assume the proposal is simply
      -- a string.
    , votingPeriodDuration :: SlotNo
    }
  deriving (Eq, Show, Generic, NFData, NoThunks, ToJSON)

mkProposal :: Era era => Text -> SlotNo -> Proposal era
mkProposal someText duration =
  Proposal
    { proposalTextHash     = Cardano.hashWithSerialiser toCBOR someText
    , votingPeriodDuration = duration
    }

instance (Typeable era, Era era) => ToCBOR (Proposal era) where
  toCBOR (Proposal { proposalTextHash, votingPeriodDuration }) =
    encodeListLen 2 <> toCBOR proposalTextHash <> toCBOR votingPeriodDuration

instance (Typeable era, Era era) => FromCBOR (Proposal era) where
  fromCBOR = do
    decodeListLenOf 2
    h <- fromCBOR
    d <- fromCBOR
    return $! Proposal h d

deriving instance Era era => FromJSON (Proposal era)

--------------------------------------------------------------------------------
-- Identifiable instance
--------------------------------------------------------------------------------

instance Era era => Identifiable (Proposal era) where
  newtype Id (Proposal era) =
    ProposalId { unProposalId :: Hash era (Proposal era) }
    deriving (Eq, Ord, Show, Generic, NFData, NoThunks, ToJSON, ToJSONKey, FromJSONKey)

  _id = ProposalId . Cardano.hashWithSerialiser toCBOR

deriving instance Era era => FromJSON (Id (Proposal era))

instance (Typeable era, Era era) => ToCBOR (Id (Proposal era)) where
  toCBOR (ProposalId hash) = toCBOR hash

instance (Typeable era, Era era) => FromCBOR (Id (Proposal era)) where
  fromCBOR = ProposalId <$> fromCBOR

--------------------------------------------------------------------------------
-- Signable instances
--------------------------------------------------------------------------------

instance Signed (Proposal era) where
  -- This is an mismatch between Shelley and the update mechanism design: the
  -- update mechanism requires that we can verify the proposal's signature,
  -- however in the Shelley design, signing the payload is outside the scope of
  -- other sub-systems (like the update sub-system) due to the use of the
  -- segregated witness approach.
  --
  -- Given that using segregated witnesses make sense in the context of
  -- blockchain systems it is the update sub-module that has to be adapted.
  signatureVerifies = const True
