{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.Shelley.TestUtils
  ( RewardUpdateOld (..),
    createRUpdOld,
    createRUpdOld_,
  )
where

import Cardano.Ledger.Address (RewardAcnt (getRwdCred))
import Cardano.Ledger.BaseTypes (ActiveSlotCoeff, BlocksMade (BlocksMade), BoundedRational (unboundRational), Globals (activeSlotCoeff), NonNegativeInterval, ProtVer, ShelleyBase, UnitInterval, activeSlotVal)
import Cardano.Ledger.Coin (Coin (Coin, unCoin), DeltaCoin (DeltaCoin), rationalToCoinViaFloor, toDeltaCoin)
import Cardano.Ledger.Compactible (Compactible (fromCompact))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential (Credential (KeyHashObj, ScriptHashObj))
import Cardano.Ledger.Era (Crypto)
import Cardano.Ledger.Keys (KeyHash, KeyRole (StakePool, Staking))
import Cardano.Ledger.Shelley.API (EpochState (EpochState), NonMyopic, SnapShot (..), SnapShots (..))
import Cardano.Ledger.Shelley.API.Types (PoolParams (..))
import Cardano.Ledger.Shelley.EpochBoundary (Stake (Stake), maxPool, poolStake, sumAllStake)
import qualified Cardano.Ledger.Shelley.HardForks as HardForks
import Cardano.Ledger.Shelley.LedgerState (AccountState (..), DPState (..), circulation, lsDPState, rewards, updateNonMyopic)
import Cardano.Ledger.Shelley.PoolRank (Likelihood, leaderProbability, likelihood)
import Cardano.Ledger.Shelley.Rewards (StakeShare (StakeShare), leaderRew, memberRew, mkApparentPerformance)
import Cardano.Ledger.Val (Val (..), invert)
import Cardano.Slotting.Slot (EpochSize)
import Control.Monad.Trans.Reader (asks)
import Control.SetAlgebra (eval, (◁))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ratio ((%))
import qualified Data.Set as Set
import qualified Data.UMap as UM
import qualified Data.VMap as VMap
import GHC.Records (HasField, getField)
import Numeric.Natural (Natural)

data RewardUpdateOld crypto = RewardUpdateOld
  { deltaTOld :: !DeltaCoin,
    deltaROld :: !DeltaCoin,
    rsOld :: !(Map (Credential 'Staking crypto) Coin),
    deltaFOld :: !DeltaCoin,
    nonMyopicOld :: !(NonMyopic crypto)
  }
  deriving (Show, Eq)

createRUpdOld ::
  forall era.
  ( HasField "_d" (Core.PParams era) UnitInterval,
    HasField "_rho" (Core.PParams era) UnitInterval,
    HasField "_tau" (Core.PParams era) UnitInterval,
    HasField "_protocolVersion" (Core.PParams era) ProtVer,
    HasField "_a0" (Core.PParams era) NonNegativeInterval,
    HasField "_nOpt" (Core.PParams era) Natural
  ) =>
  EpochSize ->
  BlocksMade (Crypto era) ->
  EpochState era ->
  Coin ->
  ShelleyBase (RewardUpdateOld (Crypto era))
createRUpdOld slotsPerEpoch b es@(EpochState acnt ss ls pr _ nm) maxSupply = 
  createRUpdOld_ @era slotsPerEpoch b ss reserves pr totalStake rs nm
  where
    ds = dpsDState $ lsDPState ls
    rs = UM.domain $ rewards ds
    reserves = _reserves acnt
    totalStake = circulation es maxSupply

createRUpdOld_ :: 
  forall era.
  ( HasField "_d" (Core.PParams era) UnitInterval,
    HasField "_rho" (Core.PParams era) UnitInterval,
    HasField "_tau" (Core.PParams era) UnitInterval,
    HasField "_protocolVersion" (Core.PParams era) ProtVer,
    HasField "_a0" (Core.PParams era) NonNegativeInterval,
    HasField "_nOpt" (Core.PParams era) Natural
  ) =>
  EpochSize ->
  BlocksMade (Crypto era) ->
  SnapShots (Crypto era) ->
  Coin ->
  Core.PParams era ->
  Coin ->
  Set.Set (Credential 'Staking (Crypto era)) ->
  NonMyopic (Crypto era) ->
  ShelleyBase (RewardUpdateOld (Crypto era))
createRUpdOld_ slotsPerEpoch b@(BlocksMade b') ss (Coin reserves) pr totalStake rs nm = do
  asc <- asks activeSlotCoeff
  let SnapShot stake' delegs' poolParams = _pstakeGo ss
      -- reserves and rewards change
      deltaR1 =
        rationalToCoinViaFloor $
          min 1 eta
            * unboundRational (getField @"_rho" pr)
            * fromIntegral reserves
      d = unboundRational (getField @"_d" pr)
      expectedBlocks =
        floor $
          (1 - d) * unboundRational (activeSlotVal asc) * fromIntegral slotsPerEpoch
      -- TODO asc is a global constant, and slotsPerEpoch should not change often at all,
      -- it would be nice to not have to compute expectedBlocks every epoch
      eta
        | unboundRational (getField @"_d" pr) >= 0.8 = 1
        | otherwise = blocksMade % expectedBlocks
      Coin rPot = _feeSS ss <> deltaR1
      deltaT1 = floor $ unboundRational (getField @"_tau" pr) * fromIntegral rPot
      _R = Coin $ rPot - deltaT1
      (rs_, newLikelihoods) =
        rewardOld
          pr
          b
          _R
          rs
          poolParams
          stake'
          delegs'
          totalStake
          asc
          slotsPerEpoch
      deltaR2 = _R <-> Map.foldr (<+>) mempty rs_
      blocksMade = fromIntegral $ Map.foldr (+) 0 b' :: Integer
  pure $
    RewardUpdateOld
      { deltaTOld = DeltaCoin deltaT1,
        deltaROld = invert (toDeltaCoin deltaR1) <> toDeltaCoin deltaR2,
        rsOld = rs_,
        deltaFOld = invert (toDeltaCoin $ _feeSS ss),
        nonMyopicOld = updateNonMyopic nm _R newLikelihoods
      }

rewardOld ::
  forall era.
  ( HasField "_d" (Core.PParams era) UnitInterval,
    HasField "_protocolVersion" (Core.PParams era) ProtVer,
    HasField "_a0" (Core.PParams era) NonNegativeInterval,
    HasField "_nOpt" (Core.PParams era) Natural
  ) =>
  Core.PParams era ->
  BlocksMade (Crypto era) ->
  Coin ->
  Set.Set (Credential 'Staking (Crypto era)) ->
  VMap.VMap VMap.VB VMap.VB (KeyHash 'StakePool (Crypto era)) (PoolParams (Crypto era)) ->
  Stake (Crypto era) ->
  VMap.VMap VMap.VB VMap.VB (Credential 'Staking (Crypto era)) (KeyHash 'StakePool (Crypto era)) ->
  Coin ->
  ActiveSlotCoeff ->
  EpochSize ->
  ( Map
      (Credential 'Staking (Crypto era))
      Coin,
    Map (KeyHash 'StakePool (Crypto era)) Likelihood
  )
rewardOld
  pp
  (BlocksMade b)
  r
  addrsRew
  poolParams
  stake
  delegs
  (Coin totalStake)
  asc
  slotsPerEpoch = (rewards', hs)
    where
      totalBlocks = sum b
      Coin activeStake = sumAllStake stake
      results :: [(KeyHash 'StakePool (Crypto era), Maybe (Map (Credential 'Staking (Crypto era)) Coin), Likelihood)]
      results = do
        (hk, pparams) <- VMap.toAscList poolParams
        let sigma = if totalStake == 0 then 0 else fromIntegral pstake % fromIntegral totalStake
            sigmaA = if activeStake == 0 then 0 else fromIntegral pstake % fromIntegral activeStake
            blocksProduced = Map.lookup hk b
            actgr = poolStake hk delegs stake
            Coin pstake = sumAllStake actgr
            rewardMap = case blocksProduced of
              Nothing -> Nothing -- This is equivalent to calling rewarOnePool with n = 0
              Just n ->
                Just $
                  rewardOnePool
                    pp
                    r
                    n
                    totalBlocks
                    pparams
                    actgr
                    sigma
                    sigmaA
                    (Coin totalStake)
                    addrsRew
            ls =
              likelihood
                (fromMaybe 0 blocksProduced)
                (leaderProbability asc sigma (getField @"_d" pp))
                slotsPerEpoch
        pure (hk, rewardMap, ls)
      f =
        if HardForks.aggregatedRewards pp
          then Map.unionsWith (<>)
          else Map.unions
      rewards' = f $ mapMaybe (\(_, x, _) -> x) results
      hs = Map.fromList $ fmap (\(hk, _, l) -> (hk, l)) results

rewardOnePool ::
  ( HasField "_d" (Core.PParams era) UnitInterval,
    HasField "_a0" (Core.PParams era) NonNegativeInterval,
    HasField "_nOpt" (Core.PParams era) Natural,
    HasField "_protocolVersion" (Core.PParams era) ProtVer
  ) =>
  Core.PParams era ->
  Coin ->
  Natural ->
  Natural ->
  PoolParams (Crypto era) ->
  Stake (Crypto era) ->
  Rational ->
  Rational ->
  Coin ->
  Set.Set (Credential 'Staking (Crypto era)) ->
  Map (Credential 'Staking (Crypto era)) Coin
rewardOnePool
  pp
  r
  blocksN
  blocksTotal
  pool
  (Stake stake)
  sigma
  sigmaA
  (Coin totalStake)
  addrsRew =
    rewards'
    where
      Coin ostake =
        Set.foldl'
          (\c o -> maybe c (mappend c . fromCompact) $ VMap.lookup (KeyHashObj o) stake)
          mempty
          (_poolOwners pool)
      Coin pledge = _poolPledge pool
      pr = fromIntegral pledge % fromIntegral totalStake
      Coin maxP =
        if pledge <= ostake
          then maxPool pp r sigma pr
          else mempty
      appPerf = mkApparentPerformance (getField @"_d" pp) sigmaA blocksN blocksTotal
      poolR = rationalToCoinViaFloor (appPerf * fromIntegral maxP)
      tot = fromIntegral totalStake
      mRewards =
        Map.fromList
          [ ( hk,
              memberRew
                poolR
                pool
                (StakeShare (unCoin (fromCompact c) % tot))
                (StakeShare sigma)
            )
            | (hk, c) <- VMap.toAscList stake,
              notPoolOwner hk
          ]
      notPoolOwner (KeyHashObj hk) = hk `Set.notMember` _poolOwners pool
      notPoolOwner (ScriptHashObj _) = HardForks.allowScriptStakeCredsToEarnRewards pp
      lReward =
        leaderRew
          poolR
          pool
          (StakeShare $ fromIntegral ostake % tot)
          (StakeShare sigma)
      f =
        if HardForks.aggregatedRewards pp
          then Map.insertWith (<>)
          else Map.insert
      potentialRewards =
        f (getRwdCred $ _poolRAcnt pool) lReward mRewards
      potentialRewards' =
        if HardForks.forgoRewardPrefilter pp
          then potentialRewards
          else eval (addrsRew ◁ potentialRewards)
      rewards' = Map.filter (/= Coin 0) potentialRewards'
