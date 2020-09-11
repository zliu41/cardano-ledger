{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE StandaloneKindSignatures   #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE TypeOperators              #-}

-- | Support for multiple (Shelley-based) eras in the ledger.
module Cardano.Ledger.Era
  ( Era(..),
    HashAnnotated(..),
    EraTag(..),
    EraRep(..),
    MinimalLazy,
    Minimal,
    typeOf,
    TypeRep,
    typeRep,
    (:~:)(Refl),
    TestEquality(testEquality),
    match,
    Byron,
    Shelley,
    Goguen,
  )
where


import Cardano.Binary( ToCBOR(toCBOR) )
import qualified Cardano.Ledger.Crypto as CryptoClass
import Data.Kind (Type, Constraint)
import Data.Typeable (Typeable)
import Shelley.Spec.Ledger.Val as ValClass
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.Crypto (HASH)
-- import Cardano.Ledger.EraRep
import Cardano.Binary( FromCBOR, Annotator)
import Cardano.Prelude( NoUnexpectedThunks (..) )

import Type.Reflection(typeOf, TypeRep, typeRep, (:~:)(Refl))
import Data.Type.Equality(TestEquality(testEquality) )

-- ========================================================================
-- Some possible (uninhabitted types) that might index an Era

data Byron c
data Shelley c
data Goguen

-- Use match like this to execute body when 'rep' has type (TypRep Int)
-- foo rep | Just Refl <- match @Int rep = body

match :: forall t s. Typeable t => TypeRep s -> Maybe (s :~: t)
match s = testEquality s (typeRep @t)

-- =========================================================================

type MinimalLazy :: Type -> Constraint
type MinimalLazy t =
   ( Typeable t, Show t, Eq t, NoUnexpectedThunks t,
     ToCBOR t, FromCBOR (Annotator t))

type Minimal :: Type -> Constraint
type Minimal t = (Typeable t, Show t, Eq t, NoUnexpectedThunks t, ToCBOR t, FromCBOR t)

-- ==============================================================================
-- This is experiment 3 (with an open era TypeRep (thisEra)).
-- It is a variation of Polina's experiment that adds a txforge field
-- to TxBody. It has high Stability since it requires change to only one function
-- 'consumed' in one file: LedgerState.hs

class
  ( CryptoClass.Crypto (Crypto e),
    Typeable e,
    ValClass.Val (ValueType e),  -- Multi Assets
    Minimal (Forge e)
  ) =>
  Era e
  where
  type Crypto e :: Type
  type ValueType e :: Type
  type Forge e :: Type
  thisRep :: EraRep e
  thisEra :: TypeRep e
  thisEra = typeRep @e

-- ================================================================================

class HashAnnotated a e | a -> e where
  hashAnnotated :: Era e => a -> Hash.Hash (HASH (Crypto e)) a
  default hashAnnotated :: (Era e,ToCBOR a) => a -> Hash.Hash (HASH (Crypto e)) a
  hashAnnotated = Hash.hashWithSerialiser @(HASH (Crypto e)) toCBOR

-- ==============================================
-- Tag a value with particuar Era

newtype EraTag e v = Tag { unTag:: v}
