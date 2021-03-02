module Cardano.Ledger.Pivo.Update.Payload.Types where

import Cardano.Crypto.DSIGN (VerKeyDSIGN)
import qualified Cardano.Crypto.Hash as Cardano

import Cardano.Ledger.Crypto (HASH, ADDRHASH, DSIGN)
import qualified Cardano.Ledger.Era as Era

type VKey era = VerKeyDSIGN (DSIGN (Era.Crypto era))

type Hash era a = Cardano.Hash (HASH (Era.Crypto era)) a

type VKeyHash era = Cardano.Hash (ADDRHASH (Era.Crypto era)) (VKey era)
