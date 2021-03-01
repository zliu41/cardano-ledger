{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Ledger.Pivo.Update.Payload.SIP.Proposal where

import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Control.DeepSeq (NFData ())
import NoThunks.Class (NoThunks ())
import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)

import Cardano.Binary (ToCBOR (toCBOR), FromCBOR (fromCBOR), encodeListLen, decodeListLenOf)
import qualified Cardano.Crypto.Hash as Cardano

import           Cardano.Ledger.Era (Era)

import Cardano.Ledger.Pivo.Update.Payload.Types (ProposalId)

data Proposal era =
  Proposal { dataHash :: ProposalId era
             -- ^ Hash of the proposal's data. For now we assume the proposal is
             -- simply a string.
           }
  deriving (Eq, Show, Generic, NFData, NoThunks, ToJSON)

mkProposal :: Era era => Text -> Proposal era
mkProposal someText = Proposal (Cardano.hashWithSerialiser toCBOR someText)

instance (Typeable era, Era era) => ToCBOR (Proposal era) where
  toCBOR (Proposal { dataHash }) =
    encodeListLen 1 <> toCBOR dataHash

instance (Typeable era, Era era) => FromCBOR (Proposal era) where
  fromCBOR = do
    decodeListLenOf 1
    h <- fromCBOR
    return $! Proposal h

deriving instance Era era => FromJSON (Proposal era)
