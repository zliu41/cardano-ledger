module Shelley.Spec.Ledger.Bench.Rewards (createRUpd) where

import Cardano.Slotting.EpochInfo
import Control.Monad.Reader (runReaderT)
import Data.Functor.Identity (runIdentity)
import Shelley.Spec.Ledger.BaseTypes (Globals (epochInfo))
import qualified Shelley.Spec.Ledger.LedgerState as LS
import Shelley.Spec.Ledger.STS.Chain (ChainState, chainNes, totalAda)
import Test.Shelley.Spec.Ledger.BenchmarkFunctions (B)

-- | Benchmark creating a reward update.
createRUpd ::
  Globals ->
  ChainState B ->
  LS.RewardUpdate B
createRUpd globals cs =
  runIdentity $
    runReaderT
      (LS.createRUpd epochSize bm es total)
      globals
  where
    nes = chainNes cs
    bm = LS.nesBprev nes
    es = LS.nesEs nes
    total = totalAda cs
    epochSize =
      runIdentity $
        epochInfoSize (epochInfo globals) (LS.nesEL nes)
