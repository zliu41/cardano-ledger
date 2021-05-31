{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Cardano.Ledger.Pivo.Update.Classes.HasWitnesses where

import Data.Set (Set)

-- | Witness set of a given payload.
class HasWitnesses p w where
  witnesses :: p -> Set w
