{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Pivo.Rules.Utick where

import Data.Typeable (Typeable)
import qualified Data.Map.Strict as Map
import Control.Arrow ((***))

import Control.State.Transition

import Cardano.Ledger.Update.Proposal (VoterId, EndorserId, _id)
import Cardano.Ledger.Update.Env.TracksSlotTime
  ( TracksSlotTime
  , currentSlot
  , slotsPerEpoch
  , epochFirstSlot
  , stableAfter
  )
import Cardano.Ledger.Update.Env.HasStakeDistribution (HasStakeDistribution, stakeDistribution)
import Cardano.Ledger.Update.Env.StakeDistribution (fromList, Stake (Stake))
import Cardano.Ledger.Update.Env.HasAdversarialStakeRatio
  ( HasAdversarialStakeRatio (adversarialStakeRatio)
  )
import qualified Cardano.Ledger.Update as UpdateAPI

import Cardano.Ledger.Era (Era, Crypto)

import Shelley.Spec.Ledger.LedgerState
import Shelley.Spec.Ledger.BaseTypes
import Shelley.Spec.Ledger.Slot (SlotNo)
import qualified Shelley.Spec.Ledger.EpochBoundary as Shelley (Stake (unStake), _stake, _pstakeMark)
import qualified Shelley.Spec.Ledger.Coin as Shelley (unCoin)

import Cardano.Ledger.Pivo.Update.Payload.SIP (SIP)
import qualified Cardano.Ledger.Pivo.Update.Payload.SIP as SIP
import qualified Cardano.Ledger.Pivo.Update.Payload.Implementation as IMP
import Cardano.Ledger.Pivo.Rules.Common (getUpdateEnv)
import qualified  Cardano.Ledger.Pivo.Update as Update

data UTICK era

instance (Typeable era, Era era) => STS (UTICK era) where

  type State (UTICK era) = Update.State era
    -- State (Core.EraRule "PPUP" era)
  type Signal (UTICK era) = SlotNo
  type Environment (UTICK era) = NewEpochState era
  type BaseM (UTICK era) = ShelleyBase
  type PredicateFailure (UTICK era) = ()

  initialRules = []

  transitionRules = [
    do
      TRC (env, st, slot) <- judgmentContext
      uEnv <- getUpdateEnv slot
      let tickEnv =
            TickEnv
              { updateEnv = uEnv
              , sipVotersSD
                  = Shelley._stake
                  $ Shelley._pstakeMark
                  $ esSnapshots
                  $ nesEs env
              } :: TickEnv era
      let st' = UpdateAPI.tick tickEnv (Update.unState st)
      return $! Update.State st'
    ]

instance ( Typeable era
         , Era era
         , BaseM sts ~ ShelleyBase
         ) => Embed (UTICK era) sts where
  wrapFailed = error "The update slot tick rules should not fail"

-- | Auxiliary data structure we use to call UpdateAPI.tick
data TickEnv era =
  TickEnv
    { updateEnv :: Update.Environment era
    , sipVotersSD :: Shelley.Stake (Crypto era)
    }

instance TracksSlotTime (TickEnv era) where
  currentSlot = currentSlot . updateEnv

  slotsPerEpoch = slotsPerEpoch . updateEnv

  epochFirstSlot = epochFirstSlot . updateEnv

  stableAfter = stableAfter . updateEnv

instance HasAdversarialStakeRatio (TickEnv era) where
  adversarialStakeRatio = const 0.35
    -- todo: this should be made configurable

instance HasStakeDistribution
           (TickEnv era)
           (VoterId (SIP era)) where
  -- fixme: this conversion is unacceptable on mainnet. To remove all the type
  -- casts it seems we'd need to:
  --
  -- - Try to make VoterId an injective type family, so that we don't need to
  --   wrap the credentials on this constructor. Probably we should try to make
  --   all type families in the Update API injective.
  --
  -- - Make the stake distribution parametric on the stake type.
  stakeDistribution env
    = fromList
    $ fmap (_id . SIP.SIPVoter *** Stake . fromInteger . Shelley.unCoin) -- fixme: unsafe Integer to Word64 conversion
    $ Map.toList
    $ Shelley.unStake
    $ sipVotersSD env

instance HasStakeDistribution
           (TickEnv era)
           (VoterId (IMP.Implementation era)) where
  stakeDistribution env
    = fromList
    $ fmap (_id . IMP.ImplVoter *** Stake . fromInteger . Shelley.unCoin) -- fixme: unsafe Integer to Word64 conversion
    $ Map.toList
    $ Shelley.unStake
    $ sipVotersSD env

instance HasStakeDistribution
           (TickEnv era)
           (EndorserId (IMP.Protocol (IMP.Implementation era))) where
  stakeDistribution env
    = fromList
    $ fmap (_id . IMP.ImplEndorser *** Stake . fromInteger . Shelley.unCoin) -- fixme: unsafe Integer to Word64 conversion
    $ Map.toList
    $ Shelley.unStake
    $ sipVotersSD env
