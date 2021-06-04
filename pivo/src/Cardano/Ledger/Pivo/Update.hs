{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Pivo.Update
  ( Payload ( Payload
            , sipSubmissions
            , sipRevelations
            , sipVotes
            , impSubmissions
            , impRevelations
            , impVotes
            , endorsements
            )
  , witnesses
  , Environment ( Environment
                , currentSlot
                , maxVotingPeriods
                , slotsPerEpoch
                , epochFirstSlot
                , stabilityWindow
                )
  , State (State, unState)
  , PredicateFailure (UpdateAPIFailure) -- It's important to expose this so that other
                                        -- modules can define a "ToObject" instance.
  )
where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData ())
import NoThunks.Class (NoThunks ())
import Data.Typeable (Typeable)
import Data.Text (Text)
import Data.Default.Class (Default, def)
import Data.Sequence.Strict (StrictSeq (Empty))

import Data.Aeson (ToJSON, FromJSON)

import Cardano.Binary
  ( FromCBOR(fromCBOR)
  , ToCBOR(toCBOR)
  , decodeListLenOf
  , encodeListLen
  )
import Data.Coders (encodeFoldable, decodeStrictSeq)
import Cardano.Slotting.Slot (SlotNo)

import qualified Cardano.Ledger.Update as USS -- Update sub-system

import Cardano.Ledger.Update.Env.HasVotingPeriodsCap
  ( HasVotingPeriodsCap
  , VotingPeriod
  )
import Cardano.Ledger.Update.Env.TracksSlotTime
  ( TracksSlotTime
  )

import qualified Cardano.Ledger.Update.Env.HasVotingPeriodsCap
import qualified Cardano.Ledger.Update.Env.TracksSlotTime

import Cardano.Ledger.Era (Crypto, Era)

import Shelley.Spec.Ledger.Keys (KeyHash, KeyRole (Witness))

import Cardano.Ledger.Pivo.Update.Classes.HasWitnesses (HasWitnesses, witnesses)
import Cardano.Ledger.Pivo.Update.Payload.Implementation (Implementation, protocolZero)
import Cardano.Ledger.Pivo.Update.Payload.SIP (SIP)
import qualified Cardano.Ledger.Pivo.Update.Payload.SIP as SIP
import qualified Cardano.Ledger.Pivo.Update.Payload.Implementation as IMP

import Shelley.Spec.Ledger.TxBody ()

data Payload era =
  Payload { sipSubmissions :: !(StrictSeq (SIP.Submission (SIP era)))
          , sipRevelations :: !(StrictSeq (SIP.Revelation (SIP era)))
          , sipVotes       :: !(StrictSeq (SIP.Vote (SIP era)))
          , impSubmissions :: !(StrictSeq (IMP.Submission (Implementation era)))
          , impRevelations :: !(StrictSeq (IMP.Revelation (Implementation era)))
          , impVotes       :: !(StrictSeq (IMP.Vote (Implementation era)))
          , endorsements   :: !(StrictSeq (IMP.Endorsement era))
          }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData, NoThunks, ToJSON, FromJSON)

instance Semigroup (Payload era) where
  pld0 <> pld1 =
    Payload
      { sipSubmissions = sipSubmissions pld0 <> sipSubmissions pld1
      , sipRevelations = sipRevelations pld0 <> sipRevelations pld1
      , sipVotes       = sipVotes       pld0 <> sipVotes       pld1
      , impSubmissions = impSubmissions pld0 <> impSubmissions pld1
      , impRevelations = impRevelations pld0 <> impRevelations pld1
      , impVotes       = impVotes       pld0 <> impVotes       pld1
      , endorsements   = endorsements   pld0 <> endorsements   pld1
      }

instance Monoid (Payload era) where
  mempty =
    Payload
      { sipSubmissions = Empty
      , sipRevelations = Empty
      , sipVotes       = Empty
      , impSubmissions = Empty
      , impRevelations = Empty
      , impVotes       = Empty
      , endorsements   = Empty
      }
  mappend = (<>)

instance
  (Crypto era ~ c
  ) => HasWitnesses (Payload era) (KeyHash 'Witness c) where
  witnesses =  foldMap witnesses . sipSubmissions
            <> foldMap witnesses . sipVotes
            <> foldMap witnesses . impSubmissions
            <> foldMap witnesses . impVotes
          -- TODO:  we need the witnesses of the activation and endorsement phases

--------------------------------------------------------------------------------
-- Update environment
--------------------------------------------------------------------------------

data Environment era =
    Environment
      { currentSlot      :: SlotNo
      , maxVotingPeriods :: VotingPeriod
      , slotsPerEpoch    :: SlotNo
      , epochFirstSlot   :: SlotNo
      , stabilityWindow  :: SlotNo
      }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData, NoThunks, ToJSON, FromJSON)

instance HasVotingPeriodsCap (Environment era) where
  maxVotingPeriods = maxVotingPeriods

instance TracksSlotTime (Environment era) where
  currentSlot = currentSlot

  slotsPerEpoch = slotsPerEpoch

  epochFirstSlot = epochFirstSlot

  stableAfter = stabilityWindow

--------------------------------------------------------------------------------
-- Update state
--------------------------------------------------------------------------------

-- | Update state. This is shared among all the update rules (e.g. PUP and UPEC)
newtype State era =
  State { unState :: USS.State (SIP era) (Implementation era) }
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToCBOR, FromCBOR, NFData, NoThunks, ToJSON, FromJSON)

instance Era era => Default (State era) where
  def = State $ USS.initialState protocolZero

instance
  ( Era era
  ) => USS.HasActivationState (State era)
                              (SIP era)
                              (Implementation era) where
  getActivationState = USS.getActivationState . unState

--------------------------------------------------------------------------------
-- Predicate failure
--------------------------------------------------------------------------------

data PredicateFailure era =
  UpdateAPIFailure Text -- todo: for simplicity we erase the structure of the Update API error.
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData, NoThunks)

--------------------------------------------------------------------------------
-- Serialisation instances
--------------------------------------------------------------------------------

instance (Typeable era, Era era) => ToCBOR (Payload era) where
  toCBOR Payload { sipSubmissions
                 , sipRevelations
                 , sipVotes
                 , impSubmissions
                 , impRevelations
                 , impVotes
                 , endorsements
                 }
    =  encodeListLen 7
    <> encodeFoldable sipSubmissions
    <> encodeFoldable sipRevelations
    <> encodeFoldable sipVotes
    <> encodeFoldable impSubmissions
    <> encodeFoldable impRevelations
    <> encodeFoldable impVotes
    <> encodeFoldable endorsements

instance (Typeable era, Era era) => FromCBOR (Payload era) where
  fromCBOR = do
    decodeListLenOf 7
    sipSubs  <- decodeStrictSeq fromCBOR
    sipRevs  <- decodeStrictSeq fromCBOR
    sipVotes <- decodeStrictSeq fromCBOR
    impSubs  <- decodeStrictSeq fromCBOR
    impRevs  <- decodeStrictSeq fromCBOR
    impVotes <- decodeStrictSeq fromCBOR
    ends     <- decodeStrictSeq fromCBOR
    return $! Payload sipSubs sipRevs sipVotes impSubs impRevs impVotes ends

instance Typeable era => ToCBOR (PredicateFailure era) where
  toCBOR (UpdateAPIFailure err) = toCBOR err

instance Typeable era => FromCBOR (PredicateFailure era) where
  fromCBOR = UpdateAPIFailure <$> fromCBOR
