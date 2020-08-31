{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | Support for multiple (Shelley-based) eras in the ledger.
module Cardano.Ledger.Era
  ( Era,
    Crypto,
    ValueType,
  )
where

import qualified Cardano.Ledger.Crypto as CryptoClass
import Data.Kind (Type)
import Data.Typeable (Typeable)
import Shelley.Spec.Ledger.Val as ValClass

class
  ( CryptoClass.Crypto (Crypto e),
    Typeable e,
    ValClass.Val (ValueType e)
  ) =>
  Era e
  where
  type Crypto e :: Type
  type ValueType e :: Type

class (Era era) => MA era where
  getForge :: TxBodyExtra era -> ValueType era

class (MA era) => VotingMA era where
  getVotes :: TxBodyExtra era -> Votes era

instance MA Shelley

  

type family TxBody era
type family TxBodyExtra era
instance Bal (TxBodyExtra era) => Era Shelley where
  getForge = vzero
