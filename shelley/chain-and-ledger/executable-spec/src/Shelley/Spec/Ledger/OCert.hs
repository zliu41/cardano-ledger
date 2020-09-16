{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Shelley.Spec.Ledger.OCert
  ( module Shelley.Spec.Ledger.Data.OCertData,
    ocertToSignable,
    currentIssueNo,
    slotsPerKESPeriod,
    kesPeriod,
  )
where

import Shelley.Spec.Ledger.Data.OCertData
import Control.Monad.Trans.Reader (asks)
import Data.Functor ((<&>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Word (Word64)
import Shelley.Spec.Ledger.Data.BaseTypesData
import Shelley.Spec.Ledger.Keys
  ( KeyHash,
    KeyRole (..),
    coerceKeyRole,
  )
import Shelley.Spec.Ledger.Slot (SlotNo (..))

currentIssueNo ::
  OCertEnv era ->
  (Map (KeyHash 'BlockIssuer era) Word64) ->
  -- | Pool hash
  KeyHash 'BlockIssuer era ->
  Maybe Word64
currentIssueNo (OCertEnv stPools genDelegs) cs hk
  | Map.member hk cs = Map.lookup hk cs
  | Set.member (coerceKeyRole hk) stPools = Just 0
  | Set.member (coerceKeyRole hk) genDelegs = Just 0
  | otherwise = Nothing

kesPeriod :: SlotNo -> ShelleyBase KESPeriod
kesPeriod (SlotNo s) =
  asks slotsPerKESPeriod <&> \spkp ->
    if spkp == 0
      then error "kesPeriod: slots per KES period was set to zero"
      else KESPeriod . fromIntegral $ s `div` spkp

-- | Extract the signable part of an operational certificate (for verification)
ocertToSignable :: OCert era -> OCertSignable era
ocertToSignable OCert {ocertVkHot, ocertN, ocertKESPeriod} =
  OCertSignable ocertVkHot ocertN ocertKESPeriod
