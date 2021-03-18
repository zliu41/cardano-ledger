{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Alonzo.Translation where

import Cardano.Binary (DecoderError)
import Cardano.Ledger.Alonzo (AlonzoEra)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Era hiding (Crypto)
import Cardano.Ledger.Mary (MaryEra)
import Shelley.Spec.Ledger.API (EpochState (..), NewEpochState (..))

--------------------------------------------------------------------------------
-- Translation from Mary to Alonzo
--
-- The instances below are needed by the consensus layer. Do not remove any of
-- them without coordinating with consensus.
--
-- Please add auxiliary instances and other declarations at the bottom of this
-- module, not in the list below so that it remains clear which instances the
-- consensus layer needs.
--
-- WARNING: when a translation instance currently uses the default
-- 'TranslationError', i.e., 'Void', it means the consensus layer relies on it
-- being total. Do not change it!
--------------------------------------------------------------------------------

type instance PreviousEra (AlonzoEra c) = MaryEra c

type instance TranslationContext (AlonzoEra c) = ()

instance
  (Crypto c) =>
  TranslateEra (AlonzoEra c) NewEpochState
  where
  translateEra ctxt nes =
    return $
      NewEpochState
        { nesEL = nesEL nes,
          nesBprev = nesBprev nes,
          nesBcur = nesBcur nes,
          nesEs = translateEra' ctxt $ nesEs nes,
          nesRu = nesRu nes,
          nesPd = nesPd nes
        }

--------------------------------------------------------------------------------
-- Auxiliary instances and functions
--------------------------------------------------------------------------------

instance Crypto c => TranslateEra (AlonzoEra c) EpochState where
  translateEra ctxt es =
    return
      EpochState
        { esAccountState = esAccountState es,
          esSnapshots = esSnapshots es,
          esLState = translateEra' ctxt $ esLState es,
          esPrevPp = translateEra' ctxt $ esPrevPp es,
          esPp = translateEra' ctxt $ esPp es,
          esNonMyopic = esNonMyopic es
        }
