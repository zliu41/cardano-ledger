{-# OPTIONS_GHC  -fno-warn-orphans #-}
-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE EmptyDataDecls      #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE RankNTypes          #-}

module Shelley.Spec.Ledger.ShelleyEra where

import Cardano.Ledger.EraRep
import Cardano.Ledger.Era
import Shelley.Spec.Ledger.Coin (Coin)
import qualified Cardano.Ledger.Crypto as CLD

import Cardano.Crypto.Hash (Blake2b_256)
import Cardano.Crypto.DSIGN (MockDSIGN)
import Cardano.Crypto.KES (MockKES)
import Cardano.Crypto.VRF.Mock(MockVRF)
--------------------------------------------------------------------------------
-- Shelley Era
--------------------------------------------------------------------------------

instance CLD.Crypto c => Era (Shelley c) where
  type Crypto (Shelley c) = c
  type ValueType (Shelley c) = Coin
  type Forge (Shelley c) = ()
  thisRep = Shelley

instance Era Goguen where
  type Crypto Goguen = GoguenCrypto
  type ValueType Goguen = Coin
  type Forge Goguen = Coin
  thisRep = Goguen
  thisEra = typeRep @Goguen


data GoguenCrypto

instance CLD.Crypto GoguenCrypto where
  type KES GoguenCrypto = MockKES 10
  type VRF GoguenCrypto = MockVRF
  type DSIGN GoguenCrypto = MockDSIGN
  type HASH GoguenCrypto = Blake2b_256
  type ADDRHASH GoguenCrypto = Blake2b_256




-- ===============================================================================
