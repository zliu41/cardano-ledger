{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module UpdateChainBench where

import Cardano.Crypto.DSIGN
import Cardano.Crypto.Hash (Blake2b_224, Blake2b_256)
import Cardano.Crypto.KES
import Cardano.Crypto.VRF.Praos
import Control.State.Transition.Trace
  ( TraceOrder (OldestFirst),
    preStatesAndSignals,
  )
import Control.State.Transition.Trace.Generator.QuickCheck (traceFromInitState)
import Data.Proxy
import Shelley.Spec.Ledger.API
  ( ChainDepState (..),
    ChainTransitionError (..),
    LedgerView (..),
    ShelleyState,
    currentLedgerView,
    updateChainDepState,
  )
import Shelley.Spec.Ledger.BaseTypes (Globals)
import Shelley.Spec.Ledger.BlockChain (BHeader, Block, bhbody, bheaderPrev, prevHashToNonce, pattern Block)
import Shelley.Spec.Ledger.Crypto
import Shelley.Spec.Ledger.STS.Chain (ChainState (ChainState))
import Shelley.Spec.Ledger.STS.Prtcl (PrtclState (..))
import Shelley.Spec.Ledger.STS.Tickn (TicknState (..))
import Test.QuickCheck (generate)
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (CHAIN, ConcreteCrypto)
import Test.Shelley.Spec.Ledger.Generator.Core (geConstants)
import Test.Shelley.Spec.Ledger.Generator.Genesis (genPParams)
import Test.Shelley.Spec.Ledger.Generator.Presets (genEnv)
import Test.Shelley.Spec.Ledger.Generator.Trace.Chain (mkGenesisChainState)
import Test.Shelley.Spec.Ledger.Utils (testGlobals)

data ShelleyCrypto

instance Crypto ShelleyCrypto where
  type DSIGN ShelleyCrypto = Ed25519DSIGN
  type KES ShelleyCrypto = Sum7KES Ed25519DSIGN Blake2b_256
  type VRF ShelleyCrypto = PraosVRF
  type HASH ShelleyCrypto = Blake2b_256
  type ADDRHASH ShelleyCrypto = Blake2b_224

-- ==============================================
type ChainErr = ChainTransitionError ShelleyCrypto

update ::
  Globals ->
  LedgerView ShelleyCrypto ->
  BHeader ShelleyCrypto ->
  ChainDepState ShelleyCrypto ->
  Either ChainErr (ChainDepState ShelleyCrypto)
update = updateChainDepState

go :: ShelleyState ShelleyCrypto -> Either ChainErr (ChainDepState ShelleyCrypto)
go state = update testGlobals (currentLedgerView state) undefined undefined

-- =======================================================================

chainState :: ChainDepState crypto
chainState = ChainDepState tickNState prtclState NeutralNonce
  where
    tickNState :: TicknState
    tickNState = TicknState NeutralNonce NeutralNonce

    prtclState :: PrtclState crypto
    prtclState = PrtclState (Map.empty) NeutralNonce NeutralNonce

genLedgerView :: Gen (LedgerView ShelleyCrypto)
genLedgerView = LedgerView
  <$> genPParams
  <*> undefined
  <*> undefined
  <*> undefined
  <*> undefined
