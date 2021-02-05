{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Screenplay where

import qualified Cardano.Crypto.DSIGN.Class as DC
import Cardano.Crypto.KES.Class as KES
import Cardano.Crypto.VRF.Class as VRF
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Crypto (Crypto, DSIGN, KES, VRF)
import qualified Cardano.Ledger.Era as ERA
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Slotting.Slot (SlotNo)
import qualified Control.State.Transition.Extended as STS
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Shelley.Spec.Ledger.BaseTypes (Seed)
import Shelley.Spec.Ledger.BlockChain (BHBody, Block)
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.LedgerState (NewEpochState)
import Shelley.Spec.Ledger.OCert (OCertSignable)
import Shelley.Spec.Ledger.STS.Bbody (BbodyEnv, BbodyState)
import Shelley.Spec.Ledger.STS.Chain (CHAIN, ChainState, totalAda)
import Shelley.Spec.Ledger.STS.Tickn (TicknEnv, TicknState)
import Test.QuickCheck (Gen, arbitrary)
import qualified Test.QuickCheck as QC
import Test.Shelley.Spec.Ledger.Utils (applySTSTest, runShelleyBase)

newtype CoreNode = CoreNode Int
  deriving (Show, Eq, Ord)

data PoolState = PoolState { kesExpiration :: Int }
  deriving (Show, Eq, Ord)

data Pool = Pool Int PoolState
  deriving (Show, Eq, Ord)

newtype User = User Int
  deriving (Show, Eq, Ord)

data Cast = Cast
  { coreNodes :: [CoreNode],
    pools :: [Pool],
    users :: [User]
  }
  deriving (Show, Eq, Ord)

data CoreNodeAction
  = GenDelegate CoreNode
  | Proposal CoreNode
  | MIR CoreNode (Set User)
  deriving (Show, Eq, Ord)

data PoolAction
  = RegPool Pool
  | Retire Pool Int
  | NewKES Pool
  deriving (Show, Eq, Ord)

data UserAction
  = GiveLovelace User User
  | Consolidate User
  | RegisterStake User
  | Delegate User Pool
  deriving (Show, Eq, Ord)

data Action = CA CoreNodeAction | PA PoolAction | UA UserAction
  deriving (Show, Eq, Ord)

getCoreNodeAction ::
  Cast ->
  CoreNode ->
  Gen (Maybe Action)
getCoreNodeAction _ cn =
  QC.frequency
    [ (10, pure . Just . CA $ GenDelegate cn)
    , (90, pure Nothing)
    ]

getPoolAction ::
  Cast ->
  Pool ->
  Gen (Maybe Action)
getPoolAction _ p =
  QC.frequency
    [ (5, pure . Just . PA . RegPool $ p)
    , (1, Just . PA . Retire p <$> arbitrary)
    , (1, pure . Just . PA . NewKES $ p)
    , (90, pure Nothing)
    ]

getUserAction ::
  Cast ->
  User ->
  Gen (Maybe Action)
getUserAction cast user =
  QC.frequency
    [ (20, Just . UA . GiveLovelace user <$> QC.elements (users cast))
    , (5, pure . Just . UA . RegisterStake $ user)
    , (5, Just . UA . Delegate user <$> QC.elements (pools cast))
    , (70, pure Nothing)
    ]

genActions :: Cast -> Gen [Action]
genActions cast = do
  as <- getActions (getCoreNodeAction cast) (coreNodes cast)
  bs <- getActions (getPoolAction cast) (pools cast)
  cs <- getActions (getUserAction cast) (users cast)
  QC.shuffle (as <> bs <> cs)
  where
    getActions f xs = catMaybes <$> mapM f xs

newtype Scene = Scene [Action]
  deriving (Show, Eq, Ord)

genScene :: Cast -> Gen Scene
genScene cast = Scene <$> genActions cast

data Play = Play Cast [Scene]
  deriving (Show, Eq, Ord)

genPlay :: Int -> Cast -> Gen [[Action]]
genPlay n cast = sequence [genActions cast | _ <- [1 .. n]]

translate :: Play -> Gen [Block era]
translate _play = undefined

-- Example of what a user translation might look like:
--
--data UserWallet c =
--  UserWallet
--    { paymentUW :: KeyPair 'Payment C,
--      stakingUW :: KeyPair 'Staking C,
--      stakeCredUW :: Credential 'Staking C,
--      stakeAddressUW :: RewardAcnt C,
--      entAddrUW :: Addr C,
--      baseAddrUW :: Addr C,
--      ptrAddrUW :: Maybe (Addr C)
--    } deriving (Show)
--
--translateUser ::
--  Word64 ->
--  UserWallet
--translateUser w =
--  UserWallet
--    (KeyPair vkPay skPay)
--    (KeyPair vkStake skStake)
--    sc
--    (RewardAcnt Testnet sc)
--    (Addr Testnet payKH StakeRefNull)
--    (Addr Testnet payKH (StakeRefBase sc))
--    Nothing
--  where
--    (skPay, vkPay) = mkKeyPair (w, 0, 0, 1, 1)
--    (skStake, vkStake) = mkKeyPair (w, 0, 0, 1, 2)
--    sc = KeyHashObj . hashKey $ vkStake
--    payKH = KeyHashObj . hashKey $ vkPay

class AdaPots era where
  getTotalAda :: ChainState era -> Integer

instance (Crypto c) => AdaPots (ShelleyEra c) where
  getTotalAda = unCoin . totalAda

type ApplyCHAIN era =
  ( ERA.Era era,
    STS.Environment (Core.EraRule "BBODY" era) ~ BbodyEnv era,
    STS.Signal (Core.EraRule "BBODY" era) ~ Block era,
    STS.State (Core.EraRule "BBODY" era) ~ BbodyState era,
    STS.Embed (Core.EraRule "BBODY" era) (CHAIN era),
    STS.Environment (Core.EraRule "TICK" era) ~ (),
    STS.Signal (Core.EraRule "TICKN" era) ~ Bool,
    STS.State (Core.EraRule "TICKN" era) ~ TicknState,
    STS.Embed (Core.EraRule "TICKN" era) (CHAIN era),
    STS.Environment (Core.EraRule "TICKN" era) ~ TicknEnv,
    STS.State (Core.EraRule "TICK" era) ~ NewEpochState era,
    STS.Signal (Core.EraRule "TICK" era) ~ SlotNo,
    STS.Embed (Core.EraRule "TICK" era) (CHAIN era),
    DC.Signable (DSIGN (ERA.Crypto era)) (OCertSignable (ERA.Crypto era)),
    KES.Signable (KES (ERA.Crypto era)) (BHBody (ERA.Crypto era)),
    VRF.Signable (VRF (ERA.Crypto era)) Seed
  )

processBlocks ::
  forall era.
  ApplyCHAIN era =>
  ChainState era ->
  [Block era] ->
  ChainState era
processBlocks cs [] = cs
processBlocks cs (b : bs) = processBlocks cs' bs
  where
    cs' = case runShelleyBase (applySTSTest @(CHAIN era) (STS.TRC ((), cs, b))) of
      Left e -> error $ show e
      Right cs'' -> cs''

preservationOfAda ::
  forall era.
  (ApplyCHAIN era, AdaPots era) =>
  Play ->
  ChainState era ->
  Gen Bool
preservationOfAda play initSt = do
  blocks <- translate play
  let end = processBlocks initSt blocks
  return $ getTotalAda @era initSt == getTotalAda @era end

-- Features we mighht want, but that are not supported:
--   * a notion of SlotNo or BlockNo in the Scene
--   * message passing which trigger actions in the future
