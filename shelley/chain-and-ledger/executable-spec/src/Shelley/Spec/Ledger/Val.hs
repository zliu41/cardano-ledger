{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}

module Shelley.Spec.Ledger.Val
 where

import           Cardano.Prelude (NoUnexpectedThunks(..), NFData ())
import           Data.Typeable (Typeable)
import           Cardano.Binary (ToCBOR, FromCBOR)

import           Shelley.Spec.Ledger.Coin (Coin (..))
import           Data.Map.Strict(Map)
import qualified Data.Map as Map
import           Data.Map.Internal(Map(..),balanceL,balanceR,singleton,link,splitLookup,link2)

{-
General function and type class definitions used in Value
-}

data Op = Gt | Lt | Gteq | Lteq | Neq | Equal

class (NFData t, Show t, Eq t, Typeable t, ToCBOR t, FromCBOR t, NoUnexpectedThunks t)
      => Val t  where
  vzero :: t                          -- This is an identity of vplus
  vplus :: t -> t -> t                -- This must be associative and commutative
  vnegate:: t -> t                    -- vplus x (vnegate x) == vzero
  scalev:: Integer -> t -> t          --
  voper:: Op -> t -> t -> Bool
     -- This will define a PARTIAL order using pointwise comparisons
     -- Semantic Equality (i.e. the Eq instance) should be (voperEqual)
  visZero:: t -> Bool                 -- is the argument vzero?
  vcoin :: t -> Coin                  -- get the Coin amount
  vinject :: Coin -> t                -- inject Coin into the Val instance
  vsize :: t -> Integer               -- compute size of Val instance
  -- TODO add PACK/UNPACK stuff to this class

-- | subtract Val
vminus :: (Val v) => v -> v -> v
vminus v1 v2 = vplus v1 (vnegate v2)

instance Val Integer where
  vzero = 0
  vplus x y = x+y
  vnegate x = -x
  scalev n x = n * x
  voper Gt x y = x>y
  voper Lt x y = x<y
  voper Gteq x y = x >= y
  voper Lteq x y = x <= y
  voper Neq x y = not(x==y)
  voper Equal x y = x==y
  visZero x = x==0
  vcoin x = Coin x
  vinject (Coin x) = x
  vsize _ = 1

instance (Ord k,Val t, NFData k, Show k, NoUnexpectedThunks k, Typeable k, ToCBOR k, FromCBOR k)
      => Val (Map k t) where
  vzero = Map.empty
  vplus x y = unionWithV vplus x y  -- There is an assumption that if the range is vzero, it is not stored in the Map
  vnegate x = mapV vnegate x        -- We enforce this by using our own versions of map and union: unionWithV and mapV
  scalev n x = mapV (scalev n) x
  voper op x y = pointWise (voper op) x y
  visZero x = Map.null x
  vcoin _ = Coin 0
  vinject _ = Map.empty -- TODO Should not be any Coin in map
  vsize x = fromIntegral $ Map.size x -- TODO shouldnt use this for Value

-- ================================================================
-- Operations on Map, so we cam make Map a Val instance.

-- Pointwise comparison assuming the map is the Default value everywhere except where it is defined
pointWise:: (Ord k, Val v) => (v -> v -> Bool) -> Map k v -> Map k v -> Bool
pointWise _ Tip Tip = True
pointWise p Tip (m@(Bin _ _ _ _ _)) = all (vzero `p`) m
pointWise p (m@(Bin _ _ _ _ _)) Tip = all ( `p` vzero) m
pointWise p m (Bin _ k v2 ls rs) =
   case Map.splitLookup k m of
      (lm,Just v1,rm) -> p v1 v2 && pointWise p ls lm && pointWise p rs rm
      _ -> False


-- The following functions enforce the invariant that vzero is never stored in a Map
insertWithV :: (Ord k,Val a) => (a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWithV = go
  where
    go :: (Ord k,Val a) => (a -> a -> a) -> k -> a -> Map k a -> Map k a
    go _ !kx x Tip = if visZero x then Tip else singleton kx x
    go f !kx x (Bin sy ky y l r) =
        case compare kx ky of
            LT -> balanceL ky y (go f kx x l) r
            GT -> balanceR ky y l (go f kx x r)
            EQ -> if visZero new then link2 l r else Bin sy kx new l r
               where new = f x y


{-# INLINABLE insertWithV #-}
unionWithV :: (Ord k,Val a) => (a -> a -> a) -> Map k a -> Map k a -> Map k a
unionWithV _f t1 Tip = t1
unionWithV f t1 (Bin _ k x Tip Tip) = insertWithV f k x t1
unionWithV f (Bin _ k x Tip Tip) t2 = insertWithV f k x t2
unionWithV _f Tip t2 = t2
unionWithV f (Bin _ k1 x1 l1 r1) t2 = case splitLookup k1 t2 of
  (l2, mb, r2) -> case mb of
      Nothing -> if visZero x1 then link2 l1l2 r1r2 else link k1 x1 l1l2 r1r2
      Just x2 -> if visZero new then link2 l1l2 r1r2 else link k1 new l1l2 r1r2
        where new = (f x1 x2)
    where !l1l2 = unionWithV f l1 l2
          !r1r2 = unionWithV f r1 r2
{-# INLINABLE unionWithV #-}


mapV:: (Ord k,Val a) => (a -> a) -> Map k a -> Map k a
mapV f m = Map.foldrWithKey accum Map.empty m
   where accum k v ans = if visZero new then ans else Map.insert k new ans
            where new = f v
{-# INLINABLE mapV #-}

deriving instance Val Coin
