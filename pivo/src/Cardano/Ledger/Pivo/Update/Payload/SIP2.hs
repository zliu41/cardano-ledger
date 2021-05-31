{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Cardano.Ledger.Pivo.Update.Payload.SIP2 where

import           Control.DeepSeq                                 (NFData)
import           Data.Aeson                                      (FromJSON, ToJSONKey, FromJSONKey,
                                                                  ToJSON)
import           Data.Set                                        (Set,
                                                                  singleton)
import           Data.Typeable                                   (Typeable)
import           GHC.Generics                                    (Generic)
import           NoThunks.Class                                  (NoThunks)
import Data.Text (Text)

import           Cardano.Binary                                  (FromCBOR (fromCBOR),
                                                                  ToCBOR (toCBOR),
                                                                  decodeListLenOf,
                                                                  encodeListLen)
import           Cardano.Crypto.DSIGN                            (hashVerKeyDSIGN)
import qualified Cardano.Crypto.Hash                             as Cardano
import           Cardano.Slotting.Slot (SlotNo)

import qualified Cardano.Ledger.Crypto
import           Cardano.Ledger.Era                              (Era)
import qualified Cardano.Ledger.Era                              as Era
import           Shelley.Spec.Ledger.Credential                  (Credential (KeyHashObj))

import qualified Shelley.Spec.Ledger.Keys                        as Shelley


import qualified Cardano.Ledger.Update as Update
import           Cardano.Ledger.Pivo.Update.Payload.Types        (VKey)
import           Cardano.Ledger.Update.Proposal                  (Confidence,
                                                                  Id, _id
  , Proposal ( Revelation
             , Submission
             , Vote
             , Voter
             )
  , Identifiable
  , Signed
  , Commitable (Commit)
  )
import qualified Cardano.Ledger.Update.Proposal                  as Proposal
import Cardano.Ledger.Update.Proposal ()

import qualified Shelley.Spec.Ledger.Keys                        as Shelley

import           Cardano.Ledger.Pivo.Update.Payload.Types    (Hash, VKey,
                                                                  VKeyHash)
import Cardano.Ledger.Pivo.Update.Classes.HasWitnesses                   (HasWitnesses, witnesses)


data SIP era =
  SIP
    { proposalTextHash     :: Hash era Text
      -- ^ Hash of the proposal's text. For now we assume the proposal is simply
      -- a string.
    , sipVotingPeriodDuration :: SlotNo
    }
  deriving (Eq, Show, Generic, NFData, NoThunks, ToJSON)

deriving instance Era era => FromJSON (SIP era)

mkSIP :: Era era => Text -> SlotNo -> SIP era
mkSIP someText duration =
  SIP
    { proposalTextHash     = Cardano.hashWithSerialiser toCBOR someText
    , sipVotingPeriodDuration = duration
    }

--------------------------------------------------------------------------------
-- Proposal instance
--------------------------------------------------------------------------------

instance Era era => Proposal.Proposal (SIP era) where
  data Submission (SIP era) =
    SIPSubmission
      { author :: VKeyHash era
        -- ^ Submission author. This will be compared against the revelator key
        -- in 'Revelation'.
      , commit :: Commit (Revelation (SIP era))
      }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData, NoThunks, ToJSON)

  data Revelation (SIP era) =
    SIPRevelation
      { proposal  :: SIP era
       -- ^ SIP that is being revealed.
      , revelator :: VKeyHash era
        -- ^ Revelation author. This should coincide with the submission author.
      , salt      :: Int
        -- ^ Salt used to calculate the commit.
      }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData, NoThunks, ToJSON)

  data Vote (SIP era) =
    SIPVote
      { voter      :: Voter (SIP era)
      , candidate  :: Id (SIP era)
      , confidence :: Confidence
      }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData, NoThunks, ToJSON, ToJSONKey, FromJSON)

  newtype Voter (SIP era) =
     SIPVoter { unSIPVoter :: Credential 'Shelley.Staking (Era.Crypto era) }
     deriving stock (Eq, Ord, Show, Generic)
     deriving newtype (NFData, NoThunks, ToJSON, ToJSONKey)

  revelationCommit = commit

  proposal = proposal

  votingPeriodDuration = sipVotingPeriodDuration

  candidate = candidate

  confidence = confidence

deriving instance Era era => FromJSON (Submission (SIP era))
deriving newtype instance Era era => FromJSON (Voter (SIP era))
deriving newtype instance Era era => FromJSONKey (Voter (SIP era))

mkSubmission
  :: Era era
  => VKey era
  -- ^ SIP's author.
  -> Int
  -- ^ Salt used to calculate the submission commit.
  -> SIP era
  -> Submission (SIP era)
mkSubmission vk salt sip =
  SIPSubmission
    { author = hashVerKeyDSIGN vk
    , commit = Proposal.commit (mkRevelation vk salt sip)
    }

mkRevelation
  :: Era era
  => VKey era
  -> Int
  -> SIP era
  -> Revelation (SIP era)
mkRevelation vk someSalt someSIP =
  SIPRevelation
    { proposal  = someSIP
    , revelator = hashVerKeyDSIGN vk
    , salt      = someSalt
    }

mkVote
  :: forall era
   . Era era
  => VKey era
  -> Id (SIP era)
  -> Confidence
  -> Vote (SIP era)
mkVote vk proposalId someConfidence =
  SIPVote
    { voter      = SIPVoter $ KeyHashObj $ Shelley.KeyHash $ hashVerKeyDSIGN vk
    , candidate  = proposalId
    , confidence = someConfidence
    }

--------------------------------------------------------------------------------
-- HasWitnesses instances
--------------------------------------------------------------------------------

instance
  (Era.Crypto era ~ c) =>
  HasWitnesses (Submission (SIP era))
               (Shelley.KeyHash 'Shelley.Witness c) where
  witnesses = singleton . Shelley.KeyHash . author

-- instance
--   HasWitnesses (Vote (SIP era))
--                (Shelley.KeyHash 'Shelley.Witness (Era.Crypto era)) where
--   witnesses v =
--     case voter v of
--       KeyHashObj vkeyHash -> singleton $ Shelley.coerceKeyRole vkeyHash
--       _                   -> mempty

--------------------------------------------------------------------------------
-- Commitable instance
--------------------------------------------------------------------------------

instance Era era => Commitable (Revelation (SIP era)) where
  type Commit (Revelation (SIP era)) =
    ( VKeyHash era -- Commit author
    , Hash era (Int, VKeyHash era, Hash era (SIP era))
    )

  commit r =
    ( revelator r
    , Cardano.hashWithSerialiser toCBOR
      $ ( salt r
        , revelator r
        , unSIPId $ _id $ proposal r
        )
    )

--------------------------------------------------------------------------------
-- Identifiable instances
--------------------------------------------------------------------------------

instance Era era => Identifiable (SIP era) where
  newtype Id (SIP era) =
    SIPId { unSIPId :: Hash era (SIP era) }
    deriving (Eq, Ord, Show, Generic, NFData, NoThunks, ToJSON, ToJSONKey, FromJSONKey)

  _id = SIPId . Cardano.hashWithSerialiser toCBOR

deriving newtype instance Era era => FromJSON (Id (SIP era))


instance Identifiable (Voter (SIP era)) where
  newtype Id (Voter (SIP era)) =
    VoterId { unVoterId :: Voter (SIP era)}
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (NFData, NoThunks, ToJSON, ToJSONKey, FromJSONKey, FromJSON)

  _id = VoterId

--------------------------------------------------------------------------------
-- Signed instances
--------------------------------------------------------------------------------

instance Signed (SIP era) where
  -- This is an mismatch between Shelley and the update mechanism design: the
  -- update mechanism requires that we can verify the proposal's signature,
  -- however in the Shelley design, signing the payload is outside the scope of
  -- other sub-systems (like the update sub-system) due to the use of the
  -- segregated witness approach.
  --
  -- Given that using segregated witnesses make sense in the context of
  -- blockchain systems it is the update sub-module that has to be adapted.
  signatureVerifies = const True

instance Signed (Submission (SIP era)) where
  -- See comment in the Signed instance for @SIP era@.
  signatureVerifies = const True

instance Signed (Vote (SIP era)) where
  -- See comment in the Signed instance for @SIP era@.
  signatureVerifies = const True

--------------------------------------------------------------------------------
-- Serialisation instances
--------------------------------------------------------------------------------

instance (Typeable era, Era era) => ToCBOR (SIP era) where
  toCBOR (SIP { proposalTextHash, sipVotingPeriodDuration }) =
    encodeListLen 2 <> toCBOR proposalTextHash <> toCBOR sipVotingPeriodDuration

instance (Typeable era, Era era) => FromCBOR (SIP era) where
  fromCBOR = do
    decodeListLenOf 2
    h <- fromCBOR
    d <- fromCBOR
    return $! SIP h d

-- TODO: make these instances equal to their counterparts
instance (Typeable era, Era era) => ToCBOR (Id (SIP era)) where
  toCBOR (SIPId hash) = toCBOR hash

instance (Typeable era, Era era) => FromCBOR (Id (SIP era)) where
  fromCBOR = SIPId <$> fromCBOR

--------------------------------------------------------------------------------
-- Payload wrapping functions
--------------------------------------------------------------------------------

wrapSIPSubmission
  :: Submission (SIP era) -> Update.Payload (SIP era) impl
wrapSIPSubmission = Update.Ideation . Proposal.Submit

wrapSIPRevelation
  :: Revelation (SIP era) -> Update.Payload (SIP era) impl
wrapSIPRevelation = Update.Ideation . Proposal.Reveal

wrapSIPVote
  :: Vote (SIP era) -> Update.Payload (SIP era) impl
wrapSIPVote = Update.Ideation . Proposal.Cast
