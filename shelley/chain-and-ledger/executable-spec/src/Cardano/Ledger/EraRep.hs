{-# LANGUAGE GADTs            #-}
{-# LANGUAGE EmptyDataDecls   #-}

-- | Support for multiple (Shelley-based) eras in the ledger.

module Cardano.Ledger.EraRep
  ( EraRep(..),
    EraTag(..),
    Shelley,
    Byron,
    Goguen,
  )
where

import qualified Cardano.Ledger.Crypto as CryptoClass

-- ==================================================
-- We introduce an uninhabited type for each new Era

data Byron c
data Shelley c
data Goguen

-- ==========================================================
-- A singleton type with a constructor for each Era instance

data EraRep era where
  Byron:: CryptoClass.Crypto c => EraRep (Byron c)
  Shelley:: CryptoClass.Crypto c => EraRep (Shelley c)
  Goguen:: EraRep Goguen

-- ==============================================
-- Tag a value with particuar Era

newtype EraTag e v = Tag { unTag:: v}
