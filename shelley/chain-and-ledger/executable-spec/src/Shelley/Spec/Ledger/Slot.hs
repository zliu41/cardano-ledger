{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Shelley.Spec.Ledger.Slot
  ( module Shelley.Spec.Ledger.Data.SlotData,
    (-*),
    (+*),
    (*-),
    epochInfoEpoch,
    epochInfoFirst,
    epochInfoSize,
  )
where

import Shelley.Spec.Ledger.Data.SlotData
import Cardano.Slotting.Block (BlockNo (..))
import Cardano.Slotting.EpochInfo (EpochInfo)
import qualified Cardano.Slotting.EpochInfo as EI
import Cardano.Slotting.Slot (EpochNo (..), EpochSize (..), SlotNo (..))
import Control.Monad.Trans (lift)
import Data.Functor.Identity (Identity)
import GHC.Stack (HasCallStack)
import Shelley.Spec.Ledger.Data.BaseTypesData (ShelleyBase)


(-*) :: SlotNo -> SlotNo -> Duration
(SlotNo s) -* (SlotNo t) = Duration (if s > t then s - t else t - s)

(+*) :: SlotNo -> Duration -> SlotNo
(SlotNo s) +* (Duration d) = SlotNo (s + d)

-- | Subtract a duration from a slot
(*-) :: SlotNo -> Duration -> SlotNo
(SlotNo s) *- (Duration d) = SlotNo (if s > d then s - d else 0)

epochInfoEpoch ::
  HasCallStack =>
  EpochInfo Identity ->
  SlotNo ->
  ShelleyBase EpochNo
epochInfoEpoch ei = lift . EI.epochInfoEpoch ei

epochInfoFirst ::
  HasCallStack =>
  EpochInfo Identity ->
  EpochNo ->
  ShelleyBase SlotNo
epochInfoFirst ei = lift . EI.epochInfoFirst ei

epochInfoSize ::
  HasCallStack =>
  EpochInfo Identity ->
  EpochNo ->
  ShelleyBase EpochSize
epochInfoSize ei = lift . EI.epochInfoSize ei
