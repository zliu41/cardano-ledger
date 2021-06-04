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
import Data.Set (singleton)

import Cardano.Crypto.DSIGN (hashVerKeyDSIGN)
import Cardano.Binary (ToCBOR (toCBOR), FromCBOR (fromCBOR), encodeListLen, decodeListLenOf)
import qualified Cardano.Crypto.Hash as Cardano

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
import Cardano.Ledger.Pivo.Update.Classes.HasWitnesses (HasWitnesses, witnesses)

import qualified Cardano.Ledger.Update.Proposal as Proposal
import qualified Cardano.Ledger.Update as Update

import Cardano.Ledger.Era (Era)

import qualified Cardano.Ledger.Era as Era

import Shelley.Spec.Ledger.Credential (Credential (KeyHashObj))
import Shelley.Spec.Ledger.PParams (PParamsUpdate, emptyPParamsUpdate)

import qualified Shelley.Spec.Ledger.Keys as Shelley

import Cardano.Ledger.Pivo.Update.Payload.Types (Hash, VKeyHash, VKey)

import  Cardano.Ledger.Pivo.Update.Payload.SIP (SIP, Id(SIPId))

data Implementation era =
  Implementation
    { sipId                    :: Hash era (SIP era)
      -- ^ Id of the SIP that this implementation implements.
    , implVotingPeriodDuration :: SlotNo
    , implProtocol             :: Protocol (Implementation era)
    }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData, NoThunks, ToJSON)

deriving anyclass instance Era era => FromJSON (Implementation era)

mkImplementation
  :: Hash era (SIP era)
  -> SlotNo
  -> Protocol (Implementation era)
  -> Implementation era
mkImplementation = Implementation

--------------------------------------------------------------------------------
-- Proposal instance
--------------------------------------------------------------------------------

instance Era era => Proposal (Implementation era) where
  data Submission (Implementation era) =
    ImplSubmission
      { submissionAuthor :: VKeyHash era
      , submissionCommit :: Commit (Revelation (Implementation era))
      }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData, NoThunks, ToJSON)

  data Revelation (Implementation era) =
    ImplRevelation
      { revealedImplementation :: Implementation era
      , revelator              :: VKeyHash era
      , revelationSalt         :: Int
      }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData, NoThunks, ToJSON, FromJSON)

  data Vote (Implementation era) =
    ImplVote
      { implVoter      :: Voter (Implementation era)
      , implCandidate  :: Id (Implementation era)
      , implConfidence :: Confidence
      }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData, NoThunks, ToJSON, ToJSONKey, FromJSON)

  newtype Voter (Implementation era) =
    ImplVoter { unImplVoter :: Credential 'Shelley.Staking (Era.Crypto era) }
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (NFData, NoThunks, ToJSON, ToJSONKey)

  revelationCommit = submissionCommit -- TODO: There's something wrong with the names here :/

  proposal = revealedImplementation

  votingPeriodDuration = implVotingPeriodDuration

  voter = _id . implVoter

  candidate = implCandidate

  confidence = implConfidence

deriving instance Era era => FromJSON (Submission (Implementation era))
deriving newtype instance Era era => FromJSON (Voter (Implementation era))
deriving newtype instance Era era => FromJSONKey (Voter (Implementation era))

mkSubmission
  :: Era era
  => VKey era
  -> Int
  -> Implementation era
  -> Submission (Implementation era)
mkSubmission vk salt impl =
  ImplSubmission
    { submissionAuthor = hashVerKeyDSIGN vk
    , submissionCommit = Proposal.commit (mkRevelation vk salt impl)
    }

mkRevelation
  :: Era era
  => VKey era
  -> Int
  -> Implementation era
  -> Revelation (Implementation era)
mkRevelation vk salt impl =
  ImplRevelation
    { revealedImplementation = impl
    , revelator = hashVerKeyDSIGN vk
    , revelationSalt = salt
    }

mkVote
  :: Era era
  => VKey era
  -> Id (Implementation era)
  -> Confidence
  -> Vote (Implementation era)
mkVote vk implId someConfidence =
  ImplVote
    { implVoter      = ImplVoter $ KeyHashObj $ Shelley.KeyHash $ hashVerKeyDSIGN vk
    , implCandidate  = implId
    , implConfidence = someConfidence
    }

mkProtocol
  :: Era era
  => Word
  -> Protocol (Implementation era)
  -> PParamsUpdate era
  -> Protocol (Implementation era)
mkProtocol pVersion protocol ppUpdate =
  ImplProtocol
    { implProtocolVersion   = ImplVersion pVersion
    , implSupersedesId      = _id protocol
    , implSupersedesVersion = version protocol
    , impParametersUpdate   = ppUpdate
    }

--------------------------------------------------------------------------------
-- HasWitnesses instances
--------------------------------------------------------------------------------

instance
  ( Era.Crypto era ~ c
  ) => HasWitnesses (Submission (Implementation era))
                    (Shelley.KeyHash 'Shelley.Witness c) where
  witnesses = singleton . Shelley.KeyHash . submissionAuthor

instance
  ( Era.Crypto era ~ c
  ) => HasWitnesses (Vote (Implementation era))
                    (Shelley.KeyHash 'Shelley.Witness c) where
  witnesses v =
    case unImplVoter (implVoter v) of
      KeyHashObj vkeyHash -> singleton $ Shelley.coerceKeyRole vkeyHash
      _                   -> mempty


--------------------------------------------------------------------------------
-- Commitable instance
--------------------------------------------------------------------------------

instance Era era => Commitable (Revelation (Implementation era)) where
  type Commit (Revelation (Implementation era)) =
    ( VKeyHash era -- Commit author
    , Hash era (Int, VKeyHash era, Hash era (Implementation era))
    )

  commit r =
    ( revelator r
    , Cardano.hashWithSerialiser toCBOR
      $ ( revelationSalt r
        , revelator r
        , unImplementationId $ _id $ revealedImplementation r
        )
    )

--------------------------------------------------------------------------------
-- Implementation instance
--------------------------------------------------------------------------------

instance Era era =>
         Proposal.Implementation (SIP era) (Implementation era) where
  data Protocol (Implementation era) =
    ImplProtocol
      { implProtocolVersion   :: Version (Protocol (Implementation era))
      , implSupersedesId      :: Id (Protocol (Implementation era))
      , implSupersedesVersion :: Version (Protocol (Implementation era))
      , impParametersUpdate   :: PParamsUpdate era
        -- ^ Parameters updates proposed by the new protocol.
      }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (NFData, NoThunks, ToJSON, ToJSONKey, FromJSONKey)

  newtype Application (Implementation era) =
    ImplApplication { unImplApplication :: Word }
    -- We will not make use of applications update in the prototype. We want to
    -- demonstrate the prototype can handle protocol updates. It remains to be
    -- seen if application updates is something we want to support in this
    -- prototype.
    deriving stock (Show, Eq, Generic)
    deriving newtype (NFData, NoThunks, ToJSON, FromJSON, ToCBOR, FromCBOR)

  preProposalId = SIPId . sipId

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
    , impParametersUpdate   = emptyPParamsUpdate
    }

deriving anyclass instance Era era => FromJSON (Protocol (Implementation era))

deriving newtype instance Era era => FromJSON (Version (Protocol (Implementation era)))

deriving newtype instance Era era => FromJSON (Id (Protocol (Implementation era)))

instance Era era => Activable (Protocol (Implementation era)) where
  newtype Endorser (Protocol (Implementation era)) =
    ImplEndorser { unImplEndorser :: Credential 'Shelley.Staking (Era.Crypto era) }
    -- todo: in practice one would allow only block issuers to endorse. We allow
    -- staking keys to endorse in this prototype to keep things simple.
    deriving stock (Show, Eq, Generic)
    deriving newtype (NFData, NoThunks, ToJSON)

  newtype Version (Protocol (Implementation era)) =
    ImplVersion { unImplVersion :: Word }
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (NFData, NoThunks, ToJSON, ToJSONKey, FromJSONKey)

  version = implProtocolVersion

  supersedesId = implSupersedesId

  supersedesVersion = implSupersedesVersion

data Endorsement era =
  -- todo: we duplicate the endorsement type defined in the update subsystem to
  -- avoid adding the instances the ledger need there. The endorsement data type
  -- should be made abstract in the update subsystem.
  Endorsement
    { endorserId :: !(Id (Endorser (Protocol (Implementation era))))
    , endorsedVersion :: !(Version (Protocol (Implementation era)))
    } deriving (Show, Eq, Generic, NoThunks, ToJSON, FromJSON)

mkEndorsement
  :: Era era
  => VKey era
  -> Word
  -> Endorsement era
mkEndorsement vk pVersion =
  Endorsement
    { endorserId = _id $ ImplEndorser $ KeyHashObj $ Shelley.KeyHash $ hashVerKeyDSIGN vk
    , endorsedVersion = ImplVersion pVersion
    }

--------------------------------------------------------------------------------
-- Identifiable instances
--------------------------------------------------------------------------------

instance Era era => Identifiable (Implementation era) where
  newtype Id (Implementation era) =
    ImplementationId { unImplementationId :: Hash era (Implementation era)}
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (NFData, NoThunks, ToJSON, ToJSONKey)

  _id = ImplementationId . Cardano.hashWithSerialiser toCBOR

deriving newtype instance Era era => FromJSON (Id (Implementation era))
deriving newtype instance Era era => FromJSONKey (Id (Implementation era))

instance Identifiable (Voter (Implementation era)) where
  newtype Id (Voter (Implementation era)) =
    VoterId { unVoterId :: Voter (Implementation era) }
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (NFData, NoThunks, ToJSON, ToJSONKey, FromJSONKey, FromJSON)

  _id = VoterId

instance Era era => Identifiable (Protocol (Implementation era)) where
  newtype Id (Protocol (Implementation era)) =
    ProtocolId { unProtocolId :: Hash era (Protocol (Implementation era)) }
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (NFData, NoThunks, ToJSON)

  _id = ProtocolId . Cardano.hashWithSerialiser toCBOR

instance Identifiable (Application (Implementation era)) where
  newtype Id (Application (Implementation era)) =
    ApplicationId { unApplicationId :: Word }
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (NFData, NoThunks, ToJSON, ToCBOR, FromCBOR)

  _id = ApplicationId . unImplApplication

instance Identifiable (Endorser (Protocol (Implementation era))) where
  newtype Id (Endorser (Protocol (Implementation era))) =
    EndorserId { unEndorserId :: Credential 'Shelley.Staking (Era.Crypto era) }
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (NFData, NoThunks, ToJSON)

  _id = EndorserId . unImplEndorser

deriving newtype instance
  Era era => FromJSON (Id (Endorser (Protocol (Implementation era))))

--------------------------------------------------------------------------------
-- Signed instances
--------------------------------------------------------------------------------

instance Signed (Submission (Implementation era)) where
  signatureVerifies = const True

instance Signed (Vote (Implementation era)) where
  signatureVerifies = const True

--------------------------------------------------------------------------------
-- Serialisation instances
--------------------------------------------------------------------------------

instance
  ( Typeable era
  , Era era
  ) => ToCBOR (Implementation era) where
  toCBOR i =  encodeListLen 3
           <> toCBOR (sipId i)
           <> toCBOR (implVotingPeriodDuration i)
           <> toCBOR (implProtocol i)

instance
  ( Typeable era
  , Era era
  ) => FromCBOR (Implementation era) where
  fromCBOR = do
    decodeListLenOf 3
    si <- fromCBOR
    iv <- fromCBOR
    ip <- fromCBOR
    return $! Implementation si iv ip

instance
  ( Typeable era
  , Era era
  ) => ToCBOR (Submission (Implementation era)) where
  toCBOR s =  encodeListLen 2
           <> toCBOR (submissionAuthor s)
           <> toCBOR (submissionCommit s)

instance
  ( Typeable era
  , Era era
  ) => FromCBOR (Submission (Implementation era)) where
  fromCBOR = do
    decodeListLenOf 2
    sa <- fromCBOR
    sc <- fromCBOR
    return $! ImplSubmission sa sc

instance
  ( Typeable era
  , Era era
  ) => ToCBOR (Revelation (Implementation era)) where
  toCBOR r =  encodeListLen 3
           <> toCBOR (revealedImplementation r)
           <> toCBOR (revelator r)
           <> toCBOR (revelationSalt r)

instance
  ( Typeable era
  , Era era
  ) => FromCBOR (Revelation (Implementation era)) where
  fromCBOR = do
    decodeListLenOf 3
    ri <- fromCBOR
    rv <- fromCBOR
    rs <- fromCBOR
    return $! ImplRevelation ri rv rs

instance
  (Typeable era
  , Era era
  ) => ToCBOR (Vote (Implementation era)) where
  toCBOR v =  encodeListLen 3
           <> toCBOR (implVoter v)
           <> toCBOR (implCandidate v)
           <> toCBOR (implConfidence v)

instance
  (Typeable era
  , Era era
  ) => FromCBOR (Vote (Implementation era)) where
  fromCBOR = do
    decodeListLenOf 3
    iv  <- fromCBOR
    ica <- fromCBOR
    ico <- fromCBOR
    return $! ImplVote iv ica ico

deriving newtype instance
  Era era => ToCBOR (Voter (Implementation era))

deriving newtype instance
  Era era => FromCBOR (Voter (Implementation era))

deriving newtype instance
  Era era => ToCBOR (Id (Implementation era))

deriving newtype instance
  Era era => FromCBOR (Id (Implementation era))

deriving newtype instance
  Era era => ToCBOR (Id (Voter (Implementation era)))

deriving newtype instance
  Era era => FromCBOR (Id (Voter (Implementation era)))

instance
  ( Typeable era
  , Era era
  ) => ToCBOR (Protocol (Implementation era)) where
  toCBOR p =  encodeListLen 4
           <> toCBOR (implProtocolVersion p)
           <> toCBOR (implSupersedesId p)
           <> toCBOR (implSupersedesVersion p)
           <> toCBOR (impParametersUpdate p)

instance
  ( Typeable era
  , Era era
  ) => FromCBOR (Protocol (Implementation era)) where
  fromCBOR = do
    decodeListLenOf 4
    v   <- fromCBOR
    sId <- fromCBOR
    sV  <- fromCBOR
    pu  <- fromCBOR
    return $! ImplProtocol v sId sV pu

deriving newtype instance
  Typeable era => ToCBOR (Version (Protocol (Implementation era)))

deriving newtype instance
  Typeable era => FromCBOR (Version (Protocol (Implementation era)))

instance (Typeable era, Era era) => ToCBOR (Endorsement era) where
  toCBOR e =  encodeListLen 2
           <> toCBOR (endorserId e)
           <> toCBOR (endorsedVersion e)

instance (Typeable era, Era era) => FromCBOR (Endorsement era) where
  fromCBOR = do
    decodeListLenOf 2
    eid <- fromCBOR
    ev  <- fromCBOR
    return $! Endorsement eid ev

deriving newtype instance
 (Typeable era, Era era) => ToCBOR (Id (Protocol (Implementation era)))

deriving newtype instance
 (Typeable era, Era era) => FromCBOR (Id (Protocol (Implementation era)))

deriving newtype instance
  (Typeable era, Era era) => ToCBOR (Id (Endorser (Protocol (Implementation era))))

deriving newtype instance
  (Typeable era, Era era) => FromCBOR (Id (Endorser (Protocol (Implementation era))))

--------------------------------------------------------------------------------
-- Payload wrapping functions
--------------------------------------------------------------------------------

wrapIMPSubmission
  :: Submission (Implementation era)
  -> Update.Payload (SIP era) (Implementation era)
wrapIMPSubmission = Update.Approval . Proposal.Submit

wrapIMPRevelation
  :: Revelation (Implementation era)
  -> Update.Payload (SIP era) (Implementation era)
wrapIMPRevelation = Update.Approval . Proposal.Reveal

wrapIMPVote
  :: Vote (Implementation era)
  -> Update.Payload (SIP era) (Implementation era)
wrapIMPVote = Update.Approval . Proposal.Cast

wrapEndorsement
  :: Endorsement era
  -> Update.Payload (SIP era) (Implementation era)
wrapEndorsement e
  = Update.Activation
  $ Update.Endorsement
      { Update.endorserId      = endorserId e
      , Update.endorsedVersion = endorsedVersion e
      }
