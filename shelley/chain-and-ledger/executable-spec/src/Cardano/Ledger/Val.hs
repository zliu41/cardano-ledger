{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
-- {-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}

-- | This module defines a notion of ASSET, which has two variations
--   Ada and MulitAsset. It defines a class Val, and a GADT indexed by
--   Ada and MultiAsset, which is an instance of Val.

module Cardano.Ledger.Val
  ( Val (..),
    scale,
    Cardano.Ledger.Val.invert,
    sumVal,
    Asset (..),
    ASSET(..),
    Blessed (..),
    AssetID(..),
    PolicyID(..),
    Value,
    Coin,
  )
where

import GHC.Generics
import Cardano.Ledger.Era(Era)
import Cardano.Ledger.CannonicalMaps
import qualified Data.Map.Strict as Map
import Data.Group (Group(..),Abelian)
import Data.Aeson (FromJSON, ToJSON)
import Cardano.Binary
  ( FromCBOR,
    ToCBOR,
    encodeListLen,
    fromCBOR,
    toCBOR,
    encodeWord,
  )
import Control.DeepSeq (NFData (..))
import NoThunks.Class (NoThunks (..), ThunkInfo(..))
import Data.ByteString (ByteString)
import Shelley.Spec.Ledger.Scripts (ScriptHash)
import Shelley.Spec.Ledger.Serialization (decodeRecordSum)
import Shelley.Spec.Ledger.BaseTypes (invalidKey)
import Data.Typeable

-- ================================================================

class
  ( Abelian t,
    Eq t
  ) =>
  Val t
  where
  -- | the value with nothing in it
  zero :: t
  zero = mempty

  -- | add two value
  (<+>) :: t -> t -> t
  x <+> y = x <> y

  -- | scale a value by an Integral constant
  (<×>) :: Integral i => i -> t -> t

  -- | subtract two values
  (<->) :: t -> t -> t
  x <-> y = x <+> ((-1 :: Integer) <×> y)

  -- | Is the argument zero?
  isZero :: t -> Bool
  isZero t = t == mempty

  -- | Get the ADA present in the value (since ADA is our "blessed" currency)
  coin :: t -> Coin

  -- | Create a value containing only this amount of ADA
  inject :: Coin -> t

  -- | modify the blessed Coin part of t
  modifyCoin :: (Coin -> Coin) -> t -> t

  size :: t -> Integer -- compute size of Val instance

  -- | used to compare values pointwise. Rather than using: (v1 <= v2) use: pointwise (<=) v1 v2
  -- | If a quantity is stored in only one of 'v1' or 'v2', we use 0 for the missing quantity.
  pointwise :: (Integer -> Integer -> Bool) -> t -> t -> Bool

-- =============================================================
-- Synonyms with types fixed at (Val t). Makes calls easier
-- to read, and gives better error messages, when a mistake is made

infixl 6 <+>

infixl 6 <->

infixl 7 <×>

scale :: (Val t, Integral i) => i -> t -> t
scale i v = i <×> v

sumVal :: (Foldable t, Val v) => t v -> v
sumVal xs = foldl (<+>) mempty xs


invert :: Val t => t -> t
invert x = (-1 :: Integer) <×> x

-- =====================================================================
-- Several instances of Val will be type family instances of ASSET

data Asset = Ada | MultiAsset

class Blessed (t::Asset) where
  prezero :: ASSET t era
  precoin:: ASSET t era -> Coin
  preinject :: Coin -> ASSET t era

instance Blessed ('Ada) where
  prezero = Coin 0
  precoin (Coin c) = Coin c
  preinject (Coin c) = Coin c

instance Blessed ('MultiAsset) where
  prezero = Value 0 Map.empty
  precoin (Value c _) = Coin c
  preinject (Coin c) = Value c Map.empty

-- ==========================

-- | Asset ID
newtype AssetID = AssetID {assetID :: ByteString}
  deriving newtype
    ( Show,
      Eq,
      ToCBOR,
      FromCBOR,
      Ord,
      NoThunks,
      NFData
    )

-- | Policy ID
newtype PolicyID era = PolicyID {policyID :: ScriptHash era}
  deriving (Show, Eq, ToCBOR, FromCBOR, Ord, NoThunks, NFData)

-- | ASSET (a GADT for both Coin and Value)
data ASSET (t::Asset) era where
  Coin:: !Integer -> ASSET 'Ada era
  Value:: !Integer -> !(Map (PolicyID era) (Map AssetID Integer)) -> ASSET 'MultiAsset era

type Coin = ASSET 'Ada ()
type Value era = ASSET 'MultiAsset era

-- ===========================================================================
-- Ugly instances, since almost nothing derives for a GADT

deriving instance Show (ASSET t era)

instance Generic (ASSET 'Ada era) where
  type Rep (ASSET 'Ada era) = Rec0 Integer
  from (Coin n) = K1 n
  to (K1 n) = Coin n

instance Generic (ASSET 'MultiAsset era) where
  type Rep (ASSET 'MultiAsset era) =  Rec0 Integer :*: Rec0 (Map (PolicyID era) (Map AssetID Integer))
  from (Value c m) = K1 c :*: K1 m
  to (K1 c :*: K1 m) = Value c m

instance ToJSON (ASSET 'Ada era) where
instance FromJSON (ASSET 'Ada era) where

instance NoThunks Coin where
  noThunks ctx (Coin x) = noThunks ctx x
  showTypeOf _proxy = "Coin"

(+++) :: Maybe ThunkInfo -> Maybe ThunkInfo -> Maybe ThunkInfo
(Just(ThunkInfo xs)) +++ (Just(ThunkInfo ys)) = Just(ThunkInfo(xs <> ys))
Nothing +++ x = x
x +++ Nothing = x

instance NoThunks (Value era) where
  noThunks ctx (Value c m) = do { d <- noThunks ctx c; n <-noThunks ctx m; pure(d +++ n)}
  showTypeOf _proxy = "Value"

instance NFData (ASSET t era) where
  rnf (Coin c) = rnf c
  rnf (Value c m) = seq (rnf c) (rnf m)

instance Ord (ASSET t era) where
  compare (Coin c) (Coin d) = compare c d
  compare (Value c m) (Value d n) = case compare c d of { EQ -> compare m n; other -> other}


instance FromCBOR (ASSET 'Ada ()) where
  fromCBOR = decodeRecordSum "ASSETCoin" $
    \case
      0 -> do
        n <- fromCBOR
        pure(2,Coin n)
      k -> invalidKey k

instance (Era era) => FromCBOR (ASSET 'MultiAsset era) where
  fromCBOR = decodeRecordSum "ASSETValue" $
    \case
      1 -> do
        c <- fromCBOR
        m <- fromCBOR
        pure $ (3,Value c m)
      k -> invalidKey k

instance ToCBOR(ASSET 'Ada ()) where
  toCBOR (Coin n) = encodeListLen 3 <> encodeWord 0 <> toCBOR n

instance (Era era,Typeable era) => ToCBOR(ASSET 'MultiAsset era) where
  toCBOR (Value c m) = encodeListLen 4 <> encodeWord 1 <> toCBOR c <> toCBOR m

instance Semigroup (ASSET t era) where
  Value c m <> Value c1 m1 =
    Value (c + c1) (cannonicalMapUnion (cannonicalMapUnion (+)) m m1)
  Coin c <> Coin d = Coin(c+d)

instance Blessed t => Monoid (ASSET t era) where
  mempty = prezero

instance Blessed t => Group (ASSET t era) where
  invert (Value c m) = Value (- c) (cannonicalMap (cannonicalMap ((-1 :: Integer) *)) m)
  invert (Coin c) = Coin (-c)

instance Blessed t => Abelian (ASSET t era)

deriving instance Eq (ASSET t era)

instance Blessed t => Val (ASSET t era) where
  n <×> (Coin c) = Coin (fromIntegral n * c)
  n <×> (Value c v) = Value (fromIntegral n * c) (cannonicalMap (cannonicalMap ((fromIntegral n) *)) v)
  isZero (Coin c) = c==0
  isZero (Value c m) = c==0 && Map.null m
  coin x = precoin x
  inject c = preinject c
  size (Coin _) = 1
  size (Value _ n) = fromIntegral (Map.size n)
  modifyCoin f (Coin c) = Coin d where (Coin d) = f (Coin c)
  modifyCoin f (Value c m) = Value d m where (Coin d) = f(Coin c)
  pointwise f (Coin x) (Coin y) = f x y
  pointwise f (Value c m) (Value d n) = c==d && pointWise (pointWise f) m n
