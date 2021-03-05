{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Implementation of an SIP
module Cardano.Ledger.Pivo.Update.Payload.Implementation where

import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Control.DeepSeq (NFData ())
import NoThunks.Class (NoThunks ())
import Data.Aeson (ToJSON, FromJSON, ToJSONKey, FromJSONKey)
import Data.Text (Text)

import qualified Cardano.Crypto.Hash as Cardano
import Cardano.Binary (ToCBOR (toCBOR), FromCBOR (fromCBOR), encodeListLen, decodeListLenOf, encodeWord, decodeWord)

import           Cardano.Slotting.Slot (SlotNo)

import Cardano.Ledger.Update.Proposal
  ( Commitable
  , Commit
  , Proposal ( Revelation
             , Submission
             , Vote
             , Voter
             , revelationCommit
             , proposal
             , votingPeriodDuration
             , voter
             , candidate
             , confidence
             )
  , Identifiable (_id)
  , Signed (signatureVerifies)
  , Id
  , Confidence
  , Activable ( Endorser
              , Version
              , version
              , supersedesId
              , supersedesVersion
              )
  , preProposalId
  , implementationType
  , Protocol
  , ImplementationType (Protocol)
  , Application
  )

import qualified Cardano.Ledger.Update.Proposal as Proposal

import Cardano.Ledger.Era (Era)

import qualified Cardano.Ledger.Era as Era

import Shelley.Spec.Ledger.Credential (Credential)

import qualified Shelley.Spec.Ledger.Keys as Shelley

import Cardano.Ledger.Pivo.Update.Payload.Types (Hash, VKeyHash)

import qualified Cardano.Ledger.Pivo.Update.Payload.SIP as SIP

data Implementation era =
  Implementation
    { sipId                    :: Hash era (SIP.Proposal era)
      -- ^ Id of the SIP that this implementation implements.
    , implVotingPeriodDuration :: SlotNo
    , implProtocol             :: Protocol (Implementation era)
    }
  deriving (Eq, Show, Generic, NFData, NoThunks, ToJSON)

instance Era era => FromJSON (Implementation era)

instance Era era => Identifiable (Implementation era) where
  newtype Id (Implementation era) =
    ImplementationId { unImplementationId :: Hash era (Implementation era)}
    deriving (Eq, Ord, Show, Generic, NFData, NoThunks, ToJSON, ToJSONKey, FromJSONKey)

  _id = ImplementationId . Cardano.hashWithSerialiser toCBOR

deriving instance Era era => FromJSON (Id (Implementation era))

instance Era era => Proposal (Implementation era) where
  data Submission (Implementation era) =
    ImplSubmission
      { submissionAuthor :: VKeyHash era
      , submissionCommit :: Commit (Revelation (Implementation era))
      }
    deriving (Eq, Show, Generic, NFData, NoThunks, ToJSON)

  data Revelation (Implementation era) =
    ImplRevelation
      { revealedImplementation :: Implementation era
      , revelator              :: VKeyHash era
      , revelationSalt         :: Int
      }
    deriving (Eq, Show, Generic, NFData, NoThunks, ToJSON)

  data Vote (Implementation era) =
    ImplVote
      { implVoter      :: Voter (Implementation era)
      , implCandidate  :: Id (Implementation era)
      , implConfidence :: Confidence
      }
    deriving (Eq, Show, Generic, NFData, NoThunks, ToJSON)

  newtype Voter (Implementation era) =
    ImplVoter { unImplVoter :: Credential 'Shelley.Staking (Era.Crypto era) }
    deriving (Eq, Ord, Show, Generic, NFData, NoThunks, ToJSON)

  revelationCommit = submissionCommit -- TODO: There's something wrong with the names here :/

  proposal = revealedImplementation

  votingPeriodDuration = implVotingPeriodDuration

  voter = _id . implVoter

  candidate = implCandidate

  confidence = implConfidence

deriving instance Era era => FromJSON (Voter (Implementation era))

instance Era era => Commitable (Revelation (Implementation era)) where
  type Commit (Revelation (Implementation era)) =
    Hash era (Int, VKeyHash era, Hash era (Implementation era))

  commit r = Cardano.hashWithSerialiser toCBOR
           $ ( revelationSalt r
             , revelator r
             , unImplementationId $ _id $ revealedImplementation r
             )

instance Signed (Submission (Implementation era)) where
  signatureVerifies = const True

instance Signed (Vote (Implementation era)) where
  signatureVerifies = const True

instance Identifiable (Voter (Implementation era)) where
  newtype Id (Voter (Implementation era)) =
    VoterId { unVoterId :: Voter (Implementation era) }
    deriving (Eq, Ord, Show, Generic, NFData, NoThunks, ToJSON, ToJSONKey, FromJSONKey, FromJSON)

  _id = VoterId

--------------------------------------------------------------------------------
-- Implementation instance
--------------------------------------------------------------------------------

instance Era era =>
         Proposal.Implementation (SIP.Proposal era) (Implementation era) where
  data Protocol (Implementation era) =
    ImplProtocol
      { implProtocolVersion   :: Version (Protocol (Implementation era))
      , implSupersedesId      :: Id (Protocol (Implementation era))
      , implSupersedesVersion :: Version (Protocol (Implementation era))
      }
    deriving (Eq, Ord, Show, Generic, NFData, NoThunks, ToJSON, ToJSONKey, FromJSONKey)

  newtype Application (Implementation era) =
    ImplApplication { unImplApplication :: Word }
    -- We will not make use of applications update in the prototype. We want to
    -- demonstrate the prototype can handle protocol updates. It remains to be
    -- seen if application updates is something we want to support in this
    -- prototype.
    deriving stock (Show, Eq)
    deriving anyclass (Generic)
    deriving newtype (NFData, NoThunks, ToJSON, FromJSON, ToCBOR, FromCBOR)

  preProposalId = SIP.ProposalId . sipId

    -- We only support protocol implementations at the moment.
  implementationType = Protocol . implProtocol

-- todo: this is just a mock up implementation of the protocol zero.
protocolZero :: Era era => Protocol (Implementation era)
protocolZero =
  ImplProtocol
    { implProtocolVersion   = ImplVersion 0
    , implSupersedesId      = ProtocolId $
        Cardano.castHash $ Cardano.hashWithSerialiser toCBOR ("Priviledge Is not VOltaire" :: Text)
    , implSupersedesVersion = ImplVersion 0
    }

instance Era era => FromJSON (Protocol (Implementation era))

deriving newtype instance Era era => FromJSON (Version (Protocol (Implementation era)))

deriving newtype instance Era era => FromJSON (Id (Protocol (Implementation era)))

instance Era era => Activable (Protocol (Implementation era)) where
  newtype Endorser (Protocol (Implementation era)) =
    ImplEndorser { unImplEndorser :: Credential 'Shelley.Staking (Era.Crypto era) }
    -- todo: in practice one would allow only block issuers to endorse. We allow
    -- staking keys to endorse in this prototype to keep things simple.
    deriving (Show, NFData, Generic, Eq, NoThunks, ToJSON)

  newtype Version (Protocol (Implementation era)) =
    ImplVersion { unImplVersion :: Word }
    deriving (Eq, Ord, Show, Generic, NFData, NoThunks, ToJSON, ToJSONKey, FromJSONKey)

  version = implProtocolVersion

  supersedesId = implSupersedesId

  supersedesVersion = implSupersedesVersion

instance Era era => Identifiable (Protocol (Implementation era)) where
  newtype Id (Protocol (Implementation era)) =
    ProtocolId { unProtocolId :: Hash era (Protocol (Implementation era)) }
    deriving stock (Eq, Ord, Show)
    deriving anyclass (Generic)
    deriving newtype (NFData, NoThunks, ToJSON)

  _id = ProtocolId . Cardano.hashWithSerialiser toCBOR

instance Identifiable (Application (Implementation era)) where
  newtype Id (Application (Implementation era)) =
    ApplicationId { unApplicationId :: Word }
    deriving stock (Eq, Ord, Show)
    deriving anyclass (Generic)
    deriving newtype (NFData, NoThunks, ToJSON, ToCBOR, FromCBOR)

  _id = ApplicationId . unImplApplication

instance Identifiable (Endorser (Protocol (Implementation era))) where
  newtype Id (Endorser (Protocol (Implementation era))) =
    EndorserId { unEndorserId :: Credential 'Shelley.Staking (Era.Crypto era) }
    deriving (Eq, Ord, Show, Generic, NFData, NoThunks, ToJSON)

  _id = EndorserId . unImplEndorser

deriving instance
  Era era => FromJSON (Id (Endorser (Protocol (Implementation era))))
--------------------------------------------------------------------------------
-- Serialisation instances
--------------------------------------------------------------------------------

instance (Typeable era, Era era) => ToCBOR (Implementation era) where
  toCBOR i =  encodeListLen 3
           <> toCBOR (sipId i)
           <> toCBOR (implVotingPeriodDuration i)
           <> toCBOR (implProtocol i)

instance FromCBOR (Implementation era) where
  fromCBOR = ...


instance (Typeable era, Era era) => ToCBOR (Protocol (Implementation era)) where
  toCBOR p =  encodeListLen 3
           <> toCBOR (implProtocolVersion p)
           <> toCBOR (implSupersedesId p)
           <> toCBOR (implSupersedesVersion p)

instance (Typeable era, Era era) =>
  FromCBOR (Protocol (Implementation era)) where
  fromCBOR = do
    decodeListLenOf 3
    v   <- fromCBOR
    sId <- fromCBOR
    sV  <- fromCBOR
    return $! ImplProtocol v sId sV

deriving newtype instance
  Typeable era => ToCBOR (Version (Protocol (Implementation era)))

deriving newtype instance
  Typeable era => FromCBOR (Version (Protocol (Implementation era)))

deriving newtype instance
 (Typeable era, Era era) => ToCBOR (Id (Protocol (Implementation era)))

deriving newtype instance
 (Typeable era, Era era) => FromCBOR (Id (Protocol (Implementation era)))

deriving newtype instance
  Era era => ToCBOR (Id (Implementation era))

deriving newtype instance
  Era era => ToCBOR (Id (Voter (Implementation era)))

deriving newtype instance
  Era era => ToCBOR (Voter (Implementation era))

deriving newtype instance
  (Typeable era, Era era) => ToCBOR (Id (Endorser (Protocol (Implementation era))))
