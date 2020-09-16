{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Shelley.Spec.Ledger.Data.SlotData
  ( SlotNo (..),
    Duration (..),
    EpochNo (..),
    EpochSize (..),
    EpochInfo,
    -- Block number
    BlockNo (..),
  )
where

import Cardano.Prelude (NoUnexpectedThunks (..))
import Cardano.Slotting.Block (BlockNo (..))
import Cardano.Slotting.EpochInfo (EpochInfo)
import Cardano.Slotting.Slot (EpochNo (..), EpochSize (..), SlotNo (..))
import Data.Word (Word64)
import GHC.Generics (Generic)
import Quiet

newtype Duration = Duration {unDuration :: Word64}
  deriving (Eq, Generic, Ord, NoUnexpectedThunks, Num, Integral, Real, Enum)
  deriving (Show) via Quiet Duration

instance Semigroup Duration where
  (Duration x) <> (Duration y) = Duration $ x + y

instance Monoid Duration where
  mempty = Duration 0
  mappend = (<>)
