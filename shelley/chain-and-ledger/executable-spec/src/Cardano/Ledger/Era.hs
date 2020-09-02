{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE StandaloneKindSignatures   #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}

-- | Support for multiple (Shelley-based) eras in the ledger.
module Cardano.Ledger.Era
  ( Era,
    Crypto,
    ValueType,
    TxBody,
    HashAnnotated(..),
  )
where

import Cardano.Prelude( NoUnexpectedThunks (..) )
import Cardano.Binary( FromCBOR, ToCBOR(toCBOR), Annotator)
import qualified Cardano.Ledger.Crypto as CryptoClass
import Data.Kind (Type, Constraint)
import Data.Typeable (Typeable)
import Shelley.Spec.Ledger.Val as ValClass
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.Crypto (HASH)

-- =========================================================================

type MinimalLazy :: Type -> Constraint
type MinimalLazy t = (Typeable t, Show t, Eq t, NoUnexpectedThunks t, ToCBOR t, FromCBOR (Annotator t))

-- type Minimal :: Type -> Constraint
-- type Minimal t = (Typeable t, Show t, Eq t, NoUnexpectedThunks t, ToCBOR t, FromCBOR t)


-- ==============================================================================

class
  ( CryptoClass.Crypto (Crypto e),
    Typeable e,
    MinimalLazy (TxBody e), HashAnnotated (TxBody e) e,
    ValClass.Val (ValueType e)  -- Multi Assets
  ) =>
  Era e
  where
  type Crypto e :: Type
  type ValueType e :: Type
  type TxBody e :: Type

-- ================================================================================

class HashAnnotated a e | a -> e where
  hashAnnotated :: Era e => a -> Hash.Hash (HASH (Crypto e)) a
  default hashAnnotated :: (Era e,ToCBOR a) => a -> Hash.Hash (HASH (Crypto e)) a
  hashAnnotated = Hash.hashWithSerialiser @(HASH (Crypto e)) toCBOR
