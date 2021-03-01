{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Cardano.Ledger.Pivo.Update.Payload.SIP.Vote where

import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Control.DeepSeq (NFData ())
import NoThunks.Class (NoThunks ())
import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import Data.Set (Set, singleton)

import qualified Data.Text as T

import Cardano.Prelude (cborError)
import Cardano.Binary (ToCBOR (toCBOR), FromCBOR (fromCBOR), encodeListLen, decodeListLenOf
                      , DecoderError (DecoderErrorCustom)
                      )
import Cardano.Crypto.DSIGN (hashVerKeyDSIGN)

import Cardano.Ledger.Update.Proposal (Confidence (For, Against, Abstain))

import           Cardano.Ledger.Era (Era)
import qualified Cardano.Ledger.Era as Era
import Shelley.Spec.Ledger.Credential (Credential (KeyHashObj))

import qualified Shelley.Spec.Ledger.Keys as Shelley

import Cardano.Ledger.Pivo.Update.Payload.SIP.Proposal (dataHash, mkProposal)
import Cardano.Ledger.Pivo.Update.Payload.Types (ProposalId, VKey)

data Vote era
  = Vote
    { voter      :: Credential 'Shelley.Staking (Era.Crypto era)
    , candidate  :: ProposalId era
    , confidence :: Confidence
    } deriving (Eq, Show, Generic, NFData, NoThunks, ToJSON)

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

-- todo: define a mkVote function
mkVote
  :: forall era
   . Era era
  => VKey era
  -> Text
  -> Confidence
  -> Vote era
mkVote vk someText someConfidence =
  Vote
    { voter      = KeyHashObj $ Shelley.KeyHash $ hashVerKeyDSIGN vk
    , candidate  = dataHash $ mkProposal @era someText
    , confidence = someConfidence
    }

voteWitnesses
  :: Vote era -> Set (Shelley.KeyHash 'Shelley.Witness (Era.Crypto era))
voteWitnesses Vote { voter } =
  case voter of
    KeyHashObj vkeyHash -> singleton $ Shelley.coerceKeyRole vkeyHash
    _                   -> mempty

--------------------------------------------------------------------------------
-- Confidence orphan instances
--------------------------------------------------------------------------------

deriving instance NFData Confidence
deriving instance ToJSON Confidence
deriving instance FromJSON Confidence

-- TODO: This instance can be put in module Cardano.Ledger.Update.Proposal
deriving instance Bounded Confidence

instance ToCBOR Confidence where
  toCBOR = toCBOR . fromEnum

instance FromCBOR Confidence where
  fromCBOR = do
    n <- fromCBOR
    if fromEnum (minBound :: Confidence) <= n
       && n <= fromEnum (maxBound :: Confidence)
    then return $! toEnum n
    else cborError $ DecoderErrorCustom "Confidence"
                   $ "Unknown confidence id" <> T.pack (show n)
