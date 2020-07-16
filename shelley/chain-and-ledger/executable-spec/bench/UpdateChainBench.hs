{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE PatternSynonyms  #-}

module UpdateChainBench where


import Data.Proxy

import Control.State.Transition.Trace
  ( TraceOrder (OldestFirst),
    preStatesAndSignals
  )

import Control.State.Transition.Trace.Generator.QuickCheck(traceFromInitState)
import Test.QuickCheck(generate)

import Cardano.Crypto.Hash (ShortHash)

import Cardano.Slotting.Slot(WithOrigin(..))


import Shelley.Spec.Ledger.API.Protocol
   ( updateChainDepState,
     LedgerView(..),
     ChainDepState(..),
     ChainTransitionError(..),
     currentLedgerView
   )
import Shelley.Spec.Ledger.API.Validation (ShelleyState)
import Shelley.Spec.Ledger.BaseTypes (Globals)
import Shelley.Spec.Ledger.BlockChain (Block,BHeader,pattern Block,PrevHash(..))
import Shelley.Spec.Ledger.BlockChain(bhbody,bheaderPrev,prevHashToNonce,labHash)
import Shelley.Spec.Ledger.STS.Chain(ChainState(ChainState))
import Shelley.Spec.Ledger.STS.Prtcl(PrtclState(..))
import Shelley.Spec.Ledger.STS.Tickn(TicknState(..))
import Shelley.Spec.Ledger.Crypto(Crypto(..))


import Test.Shelley.Spec.Ledger.Address(ShelleyCrypto)
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes(ConcreteCrypto,CHAIN)
import Test.Shelley.Spec.Ledger.Generator.Core(geConstants)
import Test.Shelley.Spec.Ledger.Generator.Presets (genEnv)
import Test.Shelley.Spec.Ledger.Generator.Trace.Chain (mkGenesisChainState)
import Test.Shelley.Spec.Ledger.Utils(testGlobals)

-- ==============================================
type ChainErr =  ChainTransitionError ShelleyCrypto

update ::
  Globals ->
  LedgerView ShelleyCrypto ->
  BHeader  ShelleyCrypto ->
  ChainDepState  ShelleyCrypto ->
  Either ChainErr (ChainDepState ShelleyCrypto)
update = updateChainDepState

go :: ShelleyState ShelleyCrypto -> Either ChainErr (ChainDepState ShelleyCrypto)
go state = update testGlobals (currentLedgerView state) undefined undefined
-- =======================================================================

type TracePair =
   ( ChainState (ConcreteCrypto ShortHash),
     Shelley.Spec.Ledger.BlockChain.Block (ConcreteCrypto ShortHash)
   )

traceM :: IO (TracePair, TracePair)
traceM = do
   let p :: Proxy ShortHash
       p = Proxy
   trace <- generate $ traceFromInitState @(CHAIN ShortHash) testGlobals 20 (genEnv p) ( Just $ mkGenesisChainState (geConstants (genEnv p)))
   case preStatesAndSignals OldestFirst trace of
      (pair1: pair2 : _) -> pure(pair1,pair2)
      _other -> error ("Impossible we should always get 2 pairs: "++show(length _other))

main2 :: IO(ChainDepState(ConcreteCrypto ShortHash))
main2 = do
  pairs <- traceM
  let ((_state,(Block preheader _txs1)), (state2,(Block bheader _txs2))) = pairs
      prevNonce = prevHashToNonce (bheaderPrev(bhbody preheader))
      (ChainState newepochState cs eta0 etaV etaC etaH (At _lab)) = state2
      prtclState = PrtclState cs eta0 etaV
      ticknState = TicknState etaC etaH
      chainstate = ChainDepState prtclState ticknState (prevHashToNonce(BlockHash(labHash  _lab)))  --prevNonce
  case (updateChainDepState testGlobals (currentLedgerView newepochState) bheader chainstate) of
     Right x -> pure x
     Left x -> error (show x)
