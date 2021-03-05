{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.Pivo.Update.Payload.SIP.Vote where

import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Control.DeepSeq (NFData ())
import NoThunks.Class (NoThunks ())
import Data.Aeson (ToJSON, FromJSON)
import Data.Set (Set, singleton)

import qualified Data.Text as T

import Cardano.Prelude (cborError)
import Cardano.Binary (ToCBOR (toCBOR), FromCBOR (fromCBOR), encodeListLen, decodeListLenOf
                      , DecoderError (DecoderErrorCustom)
                      )
import Cardano.Crypto.DSIGN (hashVerKeyDSIGN)

import Cardano.Ledger.Update.Proposal (Confidence, Id)

import           Cardano.Ledger.Era (Era)
import qualified Cardano.Ledger.Era as Era
import Shelley.Spec.Ledger.Credential (Credential (KeyHashObj))

import qualified Shelley.Spec.Ledger.Keys as Shelley

import Cardano.Ledger.Pivo.Update.Payload.SIP.Proposal (Proposal)
import Cardano.Ledger.Pivo.Update.Payload.Types (VKey)

data Vote era
  = Vote
    { voter      :: Credential 'Shelley.Staking (Era.Crypto era)
    , candidate  :: Id (Proposal era)
    , confidence :: Confidence
    } deriving (Eq, Show, Generic, NFData, NoThunks, ToJSON)

type VoterId era = Credential 'Shelley.Staking (Era.Crypto era)

deriving instance Era era => FromJSON (Vote era)

instance (Typeable era, Era era) => ToCBOR (Vote era) where
  toCBOR Vote { voter, candidate, confidence }
    =  encodeListLen 3
    <> toCBOR voter
    <> toCBOR candidate
    <> toCBOR confidence

instance (Typeable era, Era era) => FromCBOR (Vote era) where
  fromCBOR = do
    decodeListLenOf 3
    v <- fromCBOR
    c <- fromCBOR
    d <- fromCBOR
    return $! Vote v c d

mkVote
  :: forall era
   . Era era
  => VKey era
  -> Id (Proposal era)
  -> Confidence
  -> Vote era
mkVote vk proposalId someConfidence =
  Vote
    { voter      = KeyHashObj $ Shelley.KeyHash $ hashVerKeyDSIGN vk
    , candidate  = proposalId
    , confidence = someConfidence
    }

voteWitnesses
  :: Vote era -> Set (Shelley.KeyHash 'Shelley.Witness (Era.Crypto era))
voteWitnesses Vote { voter } =
  case voter of
    KeyHashObj vkeyHash -> singleton $ Shelley.coerceKeyRole vkeyHash
    _                   -> mempty
