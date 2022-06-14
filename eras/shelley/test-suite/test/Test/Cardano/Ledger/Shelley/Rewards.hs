{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Currently this uses the trace mechansim to check that computing rewards has
-- a required set of properties. It works only in the Shelley Era. It could be
-- generalized, and then moved to the Generator/Trace/ directory which computes
-- property tests in all eras.
module Test.Cardano.Ledger.Shelley.Rewards (rewardTests, C, defaultMain, newEpochProp, newEpochEventsProp, ppAgg) where

import Cardano.Binary (toCBOR)
import qualified Cardano.Crypto.DSIGN as Crypto
import Cardano.Crypto.Hash (Blake2b_256, hashToBytes)
import Cardano.Crypto.Seed (mkSeedFromBytes)
import qualified Cardano.Crypto.VRF as Crypto
import Cardano.Ledger.BaseTypes
  ( BlocksMade (..),
    Globals (..),
    Network (..),
    ProtVer (..),
    ShelleyBase,
    StrictMaybe (..),
    UnitInterval,
    epochInfoPure,
    mkActiveSlotCoeff,
  )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Compactible
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Crypto (VRF)
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Keys
  ( KeyHash,
    KeyPair (..),
    KeyRole (..),
    VKey (..),
    hashKey,
    hashWithSerialiser,
    vKey,
  )
import Cardano.Ledger.Pretty (PDoc, PrettyA (..), ppMap, ppReward, ppSet)
import Cardano.Ledger.Shelley.API.Wallet (getRewardProvenance)
import Cardano.Ledger.Shelley.EpochBoundary
  ( Stake (..),
    poolStake,
    sumAllStake,
    sumStakePerPool,
  )
import Cardano.Ledger.Shelley.LedgerState
  ( EpochState (..),
    NewEpochState (..),
    PulsingRewUpdate (..),
    RewardUpdate (..),
    completeRupd,
    createRUpd,
    filterAllRewards,
  )
import Cardano.Ledger.Shelley.PParams
  ( PParams,
    PParams' (..),
    emptyPParams,
  )
import Cardano.Ledger.Shelley.RewardUpdate
  ( FreeVars (..),
    Pulser,
    RewardAns (..),
    RewardEvent,
    RewardPulser (RSLP),
  )
import Cardano.Ledger.Shelley.Rewards
  ( Reward (rewardAmount),
    aggregateRewards,
    mkPoolRewardInfo,
  )
import Cardano.Ledger.Shelley.Rules.NewEpoch (NewEpochEvent (DeltaRewardEvent, TotalRewardEvent))
import Cardano.Ledger.Shelley.Rules.Rupd (RupdEvent (..))
import qualified Cardano.Ledger.Shelley.Rules.Tick as Tick
import Cardano.Ledger.Shelley.TestUtils (RewardUpdateOld (rsOld), createRUpdOld)
import Cardano.Ledger.Shelley.TxBody (PoolParams (..), RewardAcnt (..))
import Cardano.Ledger.Slot (epochInfoSize)
import Cardano.Slotting.Slot (EpochSize (..))
import Control.Monad (replicateM)
import Control.Monad.Trans.Reader (runReader)
import Control.Provenance (preservesJust, preservesNothing, runProvM, runWithProvM)
import Control.State.Transition.Trace (SourceSignalTarget (..), getEvents, sourceSignalTargets)
import Data.Default.Class (Default (def))
import Data.Foldable (fold, foldl')
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Pulse (Pulsable (..))
import Data.Ratio ((%))
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.VMap as VMap
import Data.Word (Word64)
import GHC.Stack
import Numeric.Natural (Natural)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (C)
import Test.Cardano.Ledger.Shelley.Generator.Core (genCoin, genNatural)
import Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen ()
import Test.Cardano.Ledger.Shelley.Rules.Chain (ChainEvent (..), ChainState (..))
import Test.Cardano.Ledger.Shelley.Rules.TestChain (forAllChainTrace, forEachEpochTrace)
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.Cardano.Ledger.Shelley.Serialisation.Generators ()
import Test.Cardano.Ledger.Shelley.Utils
  ( runShelleyBase,
    testGlobals,
    unsafeBoundRational,
  )
import Test.Cardano.Ledger.TerseTools (Terse (..), tersemapdiffs)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.QuickCheck
  ( Gen,
    Property,
    arbitrary,
    choose,
    counterexample,
    elements,
    property,
    testProperty,
    withMaxSuccess,
    (===),
  )

-- ========================================================================
-- Bounds and Constants --

maxNumPools :: Int
maxNumPools = 100

maxNumMembers :: Int
maxNumMembers = 100

maxMemberLovelace :: Integer
maxMemberLovelace = 100000

maxOwnerLovelaceAbovePledge :: Integer
maxOwnerLovelaceAbovePledge = 100000

maxPoolPledeg :: Integer
maxPoolPledeg = 1000000

maxPoolCost :: Integer
maxPoolCost = 1000000

maxPoolBlocks :: Natural
maxPoolBlocks = 1000000

numberOfTests :: Int
numberOfTests = 500

decentralizationRange :: [Rational]
decentralizationRange = [0, 0.1 .. 1]

tauRange :: [Rational]
tauRange = [0, 0.05 .. 0.3]

rhoRange :: [Rational]
rhoRange = [0, 0.05 .. 0.3]

-- Helpers --

keyPair :: CC.Crypto crypto => Int -> KeyPair r crypto
keyPair seed = KeyPair vk sk
  where
    vk = VKey (Crypto.deriveVerKeyDSIGN sk)
    sk =
      Crypto.genKeyDSIGN $
        mkSeedFromBytes . hashToBytes $
          hashWithSerialiser @Blake2b_256 toCBOR seed

vrfKeyPair :: forall v. Crypto.VRFAlgorithm v => Int -> (Crypto.SignKeyVRF v, Crypto.VerKeyVRF v)
vrfKeyPair seed = (sk, vk)
  where
    vk = Crypto.deriveVerKeyVRF sk
    sk =
      Crypto.genKeyVRF $
        mkSeedFromBytes . hashToBytes $
          hashWithSerialiser @Blake2b_256 toCBOR seed

data PoolSetUpArgs crypto f = PoolSetUpArgs
  { poolPledge :: f Coin,
    poolCost :: f Coin,
    poolMargin :: f UnitInterval,
    poolMembers :: f (Map (Credential 'Staking crypto) Coin)
  }

emptySetupArgs :: PoolSetUpArgs crypto Maybe
emptySetupArgs =
  PoolSetUpArgs
    { poolPledge = Nothing,
      poolCost = Nothing,
      poolMargin = Nothing,
      poolMembers = Nothing
    }

data PoolInfo crypto = PoolInfo
  { params :: PoolParams crypto,
    coldKey :: KeyPair 'StakePool crypto,
    ownerKey :: KeyPair 'Staking crypto,
    ownerStake :: Coin,
    rewardKey :: KeyPair 'Staking crypto,
    members :: Map (Credential 'Staking crypto) Coin
  }

-- Generators --

genNonOwnerMembers :: CC.Crypto crypto => Gen (Map (Credential 'Staking crypto) Coin)
genNonOwnerMembers = do
  numMembers <- choose (0, maxNumMembers)
  fmap Map.fromList . replicateM numMembers $ do
    credential <- KeyHashObj . hashKey . vKey . keyPair <$> arbitrary
    coins <- genCoin 0 maxMemberLovelace
    pure (credential, coins)

getOrGen :: Maybe a -> Gen a -> Gen a
getOrGen (Just x) _ = pure x
getOrGen Nothing g = g

genMargin :: Gen UnitInterval
genMargin = do
  let denom = 10
  numer <- choose (0, denom)
  pure $ unsafeBoundRational (numer % denom)

genPoolInfo :: forall crypto. CC.Crypto crypto => PoolSetUpArgs crypto Maybe -> Gen (PoolInfo crypto)
genPoolInfo PoolSetUpArgs {poolPledge, poolCost, poolMargin, poolMembers} = do
  pledge <- getOrGen poolPledge $ genCoin 0 maxPoolPledeg
  cost <- getOrGen poolCost $ genCoin 0 maxPoolCost
  margin <- getOrGen poolMargin genMargin
  vrfKey <- vrfKeyPair @(VRF crypto) <$> arbitrary
  coldKey <- keyPair <$> arbitrary
  ownerKey <- keyPair <$> arbitrary
  rewardKey <- keyPair <$> arbitrary
  members' <- getOrGen poolMembers genNonOwnerMembers
  ownerStake <- (pledge <>) <$> genCoin 0 maxOwnerLovelaceAbovePledge
  -- here we are forcing the pool to meet the pledeg, later we may want flexibility
  let members = Map.insert (KeyHashObj . hashKey . vKey $ ownerKey) ownerStake members'
      params =
        PoolParams
          { _poolId = hashKey . vKey $ coldKey,
            _poolVrf = Crypto.hashVerKeyVRF . snd $ vrfKey,
            _poolPledge = pledge,
            _poolCost = cost,
            _poolMargin = margin,
            _poolRAcnt = RewardAcnt Testnet . KeyHashObj . hashKey . vKey $ rewardKey,
            _poolOwners = Set.fromList [hashKey $ vKey ownerKey],
            _poolRelays = StrictSeq.empty,
            _poolMD = SNothing
          }
  pure $ PoolInfo {params, coldKey, ownerKey, ownerStake, rewardKey, members}

genRewardPPs :: Gen (PParams era)
genRewardPPs = do
  d <- g decentralizationRange
  t <- g tauRange
  r <- g rhoRange
  pure $ emptyPParams {_d = d, _tau = t, _rho = r}
  where
    g xs = unsafeBoundRational <$> elements xs

genBlocksMade :: [PoolParams crypto] -> Gen (BlocksMade crypto)
genBlocksMade pools = BlocksMade . Map.fromList <$> mapM f pools
  where
    f p = (_poolId p,) <$> genNatural 0 maxPoolBlocks

-- Properties --

toCompactCoinError :: HasCallStack => Coin -> CompactForm Coin
toCompactCoinError c =
  fromMaybe (error $ "Invalid Coin: " <> show c) $ toCompact c

rewardsBoundedByPot ::
  forall era.
  (Era era, Core.PParams era ~ PParams era) =>
  Proxy era ->
  Property
rewardsBoundedByPot _ = property $ do
  numPools <- choose (0, maxNumPools)
  pools <- sequence $ genPoolInfo @(Crypto era) <$> replicate numPools emptySetupArgs
  pp <- genRewardPPs
  rewardPot <- genCoin 0 (fromIntegral $ maxLovelaceSupply testGlobals)
  undelegatedLovelace <- genCoin 0 (fromIntegral $ maxLovelaceSupply testGlobals)
  asc <- mkActiveSlotCoeff . unsafeBoundRational <$> elements [0.1, 0.2, 0.3]
  bs@(BlocksMade blocks) <- genBlocksMade (fmap params pools)
  let totalBlocks = sum blocks
  silentSlots <- genNatural 0 (3 * totalBlocks) -- the '3 * sum blocks' is pretty arbitrary
  let stake = fold (members <$> pools)
      delegs = fold $
        flip fmap pools $
          \PoolInfo {params, members} ->
            Map.fromList $ (,_poolId params) <$> Map.keys members
      rewardAcnts = Set.fromList $ Map.keys delegs
      poolParams =
        VMap.fromList
          [(_poolId params, params) | PoolInfo {params} <- pools]
      totalLovelace = undelegatedLovelace <> fold stake
      slotsPerEpoch = EpochSize . fromIntegral $ totalBlocks + silentSlots
      (RewardAns rs _) =
        runShelleyBase $
          reward @era
            pp
            bs
            rewardPot
            rewardAcnts
            poolParams
            (Stake (VMap.fromMap (toCompactCoinError <$> stake)))
            (VMap.fromMap delegs)
            totalLovelace
  pure $
    counterexample
      ( mconcat
          [ "pp\n",
            show pp,
            "\nrewardPot\n",
            show rewardPot,
            "\nrewardAcnts\n",
            show rewardAcnts,
            "\npoolParams\n",
            show poolParams,
            "\nstake\n",
            show stake,
            "\ndelegs\n",
            show delegs,
            "\ntotalLovelace\n",
            show totalLovelace,
            "\nasc\n",
            show asc,
            "\nslotsPerEpoch\n",
            show slotsPerEpoch
          ]
      )
      (fold (fmap rewardAmount rs) < rewardPot)

-- =================================================
-- tests when running rewards with provenance

-- Analog to getRewardProvenance, but does not produce Provenance
justRewardInfo ::
  forall era.
  (Core.PParams era ~ PParams era) =>
  Globals ->
  NewEpochState era ->
  RewardUpdate (Crypto era)
justRewardInfo globals newepochstate =
  runReader
    (runProvM $ createRUpd slotsPerEpoch blocksmade epochstate maxsupply asc k)
    globals
  where
    epochstate = nesEs newepochstate
    maxsupply :: Coin
    maxsupply = Coin (fromIntegral (maxLovelaceSupply globals))
    blocksmade :: BlocksMade (Crypto era)
    blocksmade = nesBprev newepochstate
    epochnumber = nesEL newepochstate
    slotsPerEpoch :: EpochSize
    slotsPerEpoch = runReader (epochInfoSize (epochInfoPure globals) epochnumber) globals
    asc = activeSlotCoeff globals
    k = securityParameter testGlobals

sameWithOrWithoutProvenance ::
  forall era.
  (Core.PParams era ~ PParams era) =>
  Globals ->
  NewEpochState era ->
  Property
sameWithOrWithoutProvenance globals newepochstate = with === without
  where
    (with, _) = getRewardProvenance globals newepochstate
    without = justRewardInfo globals newepochstate

nothingInNothingOut ::
  forall era.
  (Core.PParams era ~ PParams era) =>
  NewEpochState era ->
  Property
nothingInNothingOut newepochstate =
  counterexample "nothingInNothingOut fails" $
    runReader
      (preservesNothing $ createRUpd slotsPerEpoch blocksmade epochstate maxsupply asc k)
      globals
  where
    globals = testGlobals
    epochstate = nesEs newepochstate
    maxsupply :: Coin
    maxsupply = Coin (fromIntegral (maxLovelaceSupply globals))
    blocksmade :: BlocksMade (Crypto era)
    blocksmade = nesBprev newepochstate
    epochnumber = nesEL newepochstate
    slotsPerEpoch :: EpochSize
    slotsPerEpoch = runReader (epochInfoSize (epochInfoPure globals) epochnumber) globals
    asc = activeSlotCoeff globals
    k = securityParameter testGlobals

justInJustOut ::
  forall era.
  (Core.PParams era ~ PParams era) =>
  NewEpochState era ->
  Property
justInJustOut newepochstate =
  counterexample "justInJustOut fails" $
    runReader
      (preservesJust def $ createRUpd slotsPerEpoch blocksmade epochstate maxsupply asc k)
      globals
  where
    globals = testGlobals
    epochstate = nesEs newepochstate
    maxsupply :: Coin
    maxsupply = Coin (fromIntegral (maxLovelaceSupply globals))
    blocksmade :: BlocksMade (Crypto era)
    blocksmade = nesBprev newepochstate
    epochnumber = nesEL newepochstate
    slotsPerEpoch :: EpochSize
    slotsPerEpoch = runReader (epochInfoSize (epochInfoPure globals) epochnumber) globals
    asc = activeSlotCoeff globals
    k = securityParameter testGlobals

-- ====================================================================================
-- To demonstrate that the code we wrote that enables provenance collection does not
-- change the result of reward calculation. we reproduce the old style functions here.

overrideProtocolVersionUsedInRewardCalc ::
  Core.PParams era ~ PParams era =>
  ProtVer ->
  EpochState era ->
  EpochState era
overrideProtocolVersionUsedInRewardCalc pv es =
  es {esPrevPp = pp'}
  where
    pp = esPrevPp $ es
    pp' = pp {_protocolVersion = pv}

oldEqualsNew ::
  forall era.
  ( era ~ C,
    Core.PParams era ~ PParams era
  ) =>
  ProtVer ->
  NewEpochState era ->
  Property
oldEqualsNew pv newepochstate =
  counterexample
    (show (prettyA newepochstate) ++ "\n new = " ++ show new ++ "\n old = " ++ show old)
    (old === new)
  where
    globals = testGlobals
    epochstate = overrideProtocolVersionUsedInRewardCalc pv $ nesEs newepochstate
    maxsupply :: Coin
    maxsupply = Coin (fromIntegral (maxLovelaceSupply globals))
    blocksmade :: BlocksMade (Crypto era)
    blocksmade = nesBprev newepochstate
    epochnumber = nesEL newepochstate
    slotsPerEpoch :: EpochSize
    slotsPerEpoch = runReader (epochInfoSize (epochInfoPure globals) epochnumber) globals
    unAggregated =
      runReader (runProvM $ createRUpd slotsPerEpoch blocksmade epochstate maxsupply asc k) globals
    old = rsOld $ runReader (createRUpdOld slotsPerEpoch blocksmade epochstate maxsupply) globals
    new_with_zeros = aggregateRewards @(Crypto era) (emptyPParams {_protocolVersion = pv}) (rs unAggregated)
    new = Map.filter (/= Coin 0) new_with_zeros
    asc = activeSlotCoeff globals
    k = securityParameter testGlobals

oldEqualsNewOn ::
  forall era.
  ( era ~ C,
    Core.PParams era ~ PParams era
  ) =>
  ProtVer ->
  NewEpochState era ->
  Property
oldEqualsNewOn pv newepochstate = old === new
  where
    globals = testGlobals
    epochstate = overrideProtocolVersionUsedInRewardCalc pv $ nesEs newepochstate
    maxsupply :: Coin
    maxsupply = Coin (fromIntegral (maxLovelaceSupply globals))
    blocksmade :: BlocksMade (Crypto era)
    blocksmade = nesBprev newepochstate
    epochnumber = nesEL newepochstate
    slotsPerEpoch :: EpochSize
    slotsPerEpoch = runReader (epochInfoSize (epochInfoPure globals) epochnumber) globals
    (unAggregated, _) =
      runReader (runWithProvM def $ createRUpd slotsPerEpoch blocksmade epochstate maxsupply asc k) globals
    old :: Map (Credential 'Staking (Crypto era)) Coin
    old = rsOld $ runReader (createRUpdOld slotsPerEpoch blocksmade epochstate maxsupply) globals
    new_with_zeros =
      aggregateRewards @(Crypto era) (emptyPParams {_protocolVersion = pv}) (rs unAggregated)
    new = Map.filter (/= Coin 0) new_with_zeros
    asc = activeSlotCoeff globals
    k = securityParameter testGlobals

lastElem :: [a] -> Maybe a
lastElem [a] = Just a
lastElem [] = Nothing
lastElem (_ : xs) = lastElem xs

-- | Provide a legitimate NewEpochState to make an test Property
newEpochProp :: Word64 -> (NewEpochState C -> Property) -> Property
newEpochProp tracelen propf = withMaxSuccess 100 $
  forAllChainTrace @C tracelen $ \tr ->
    case lastElem (sourceSignalTargets tr) of
      Just SourceSignalTarget {target} -> propf (chainNes target)
      _ -> True === True

-- | Given a NewEpochState and [ChainEvent], test a Property at every Epoch Boundary
newEpochEventsProp :: Word64 -> ([ChainEvent C] -> NewEpochState C -> Property) -> Property
newEpochEventsProp tracelen propf = withMaxSuccess 10 $
  forEachEpochTrace @C 10 tracelen $ \tr ->
    case lastElem (sourceSignalTargets tr) of
      Just SourceSignalTarget {target} -> propf (concat (runShelleyBase $ getEvents tr)) (chainNes target)
      _ -> True === True

aggIncrementalRewardEvents :: [ChainEvent C] -> Map (Credential 'Staking (Crypto C)) (Set (Reward (Crypto C)))
aggIncrementalRewardEvents events = foldl' accum Map.empty events
  where
    accum ans (TickEvent (Tick.RupdEvent (RupdEvent _ m))) = Map.unionWith Set.union m ans
    accum ans (TickEvent (Tick.NewEpochEvent (DeltaRewardEvent (RupdEvent _ m)))) = Map.unionWith Set.union m ans
    accum ans _ = ans

getMostRecentTotalRewardEvent :: [ChainEvent C] -> Map (Credential 'Staking (Crypto C)) (Set (Reward (Crypto C)))
getMostRecentTotalRewardEvent events = foldl' accum Map.empty events
  where
    accum ans (TickEvent (Tick.NewEpochEvent (TotalRewardEvent _ m))) = Map.unionWith Set.union m ans
    accum ans _ = ans

complete :: PulsingRewUpdate crypto -> (RewardUpdate crypto, RewardEvent crypto)
complete (Complete r) = (r, mempty)
complete (Pulsing rewsnap pulser) = runShelleyBase $ runProvM (completeRupd (Pulsing rewsnap pulser))

eventsMirrorRewards :: [ChainEvent C] -> NewEpochState C -> Property
eventsMirrorRewards events nes = same eventRew compRew
  where
    (compRew, eventRew) =
      case nesRu nes of
        SNothing -> (total, aggFilteredEvent)
        SJust pulser ->
          ( Map.unionWith Set.union (rs completed) total,
            Map.unionWith Set.union lastevent aggevent
          )
          where
            (completed, lastevent) = complete pulser
    total = getMostRecentTotalRewardEvent events
    aggevent = aggIncrementalRewardEvents events
    (aggFilteredEvent, _, _, _) = filterAllRewards aggevent (nesEs nes)
    same x y = withMaxSuccess 1 $ counterexample message (x === y)
      where
        message =
          ( "events don't mirror rewards "
              ++ tersemapdiffs "Map differences: aggregated filtered events on the left, computed on the right." x y
          )

ppAgg :: Map (Credential 'Staking (Crypto C)) (Set (Reward (Crypto C))) -> PDoc
ppAgg m = ppMap prettyA (ppSet ppReward) m

instance Terse (Reward crypto) where
  terse x = show (ppReward x)

instance PrettyA x => Terse (Set x) where
  terse x = show (ppSet prettyA x)

instance PrettyA (Reward crypto) where
  prettyA = ppReward

-- ================================================================

reward ::
  forall era.
  (Core.PParams era ~ PParams era) =>
  Core.PParams era ->
  BlocksMade (Crypto era) ->
  Coin ->
  Set (Credential 'Staking (Crypto era)) ->
  VMap.VMap VMap.VB VMap.VB (KeyHash 'StakePool (Crypto era)) (PoolParams (Crypto era)) ->
  Stake (Crypto era) ->
  VMap.VMap VMap.VB VMap.VB (Credential 'Staking (Crypto era)) (KeyHash 'StakePool (Crypto era)) ->
  Coin ->
  ShelleyBase (RewardAns (Crypto era))
reward
  pp
  (BlocksMade b)
  r
  addrsRew
  poolParams
  stake
  delegs
  (Coin totalStake) = completeM pulser
    where
      totalBlocks = sum b
      stakePerPool = sumStakePerPool delegs stake
      Coin activeStake = sumAllStake stake
      -- ensure mkPoolRewardInfo does not use stake that doesn't belong to the pool
      stakeForPool pool = poolStake (_poolId pool) delegs stake
      mkPoolRewardInfo' pool =
        mkPoolRewardInfo
          pp
          r
          (BlocksMade b)
          totalBlocks
          (stakeForPool pool)
          delegs
          stakePerPool
          (Coin totalStake)
          (Coin activeStake)
          pool
      poolRewardInfo = VMap.toMap $ VMap.mapMaybe (either (const Nothing) Just . mkPoolRewardInfo') poolParams
      pp_pv = _protocolVersion pp
      free =
        FreeVars
          { addrsRew,
            totalStake,
            pp_pv,
            poolRewardInfo,
            delegs
          }
      pulser :: Pulser (Crypto era)
      pulser = RSLP 2 free (unStake stake) (RewardAns Map.empty Map.empty)

-- ==================================================================

-- | Note that chainlen must be set high enough so that enough epochs
-- have passed to get non-trivial rewards.
chainlen :: Word64
chainlen = 200

rewardTests :: TestTree
rewardTests =
  testGroup
    "Reward Tests"
    [ testProperty "Sum of rewards is bounded by reward pot" (withMaxSuccess numberOfTests (rewardsBoundedByPot (Proxy @C))),
      testProperty "provenance does not affect result" (newEpochProp 100 (sameWithOrWithoutProvenance @C testGlobals)),
      testProperty "ProvM preserves Nothing" (newEpochProp 100 (nothingInNothingOut @C)),
      testProperty "ProvM preserves Just" (newEpochProp 100 (justInJustOut @C)),
      testProperty "compare with reference impl, no provenance, v3" (newEpochProp chainlen (oldEqualsNew @C (ProtVer 3 0))),
      testProperty "compare with reference impl, no provenance, v7" (newEpochProp chainlen (oldEqualsNew @C (ProtVer 7 0))),
      testProperty "compare with reference impl, with provenance" (newEpochProp chainlen (oldEqualsNewOn @C (ProtVer 3 0))),
      testProperty "delta events mirror reward updates" (newEpochEventsProp chainlen eventsMirrorRewards)
    ]
