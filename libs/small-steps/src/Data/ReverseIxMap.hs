{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.ReverseIxMap
 ( ReverseIxMap
 , IsPair (..)
 , lookup
 , lookupReverse
 , insert
 , delete
 , mapMaybe
 , mapMaybeWithKey
 , empty
 , getForward
 , getBackward
 , fromList
 , foldrWithKey
 )
 where

import Prelude hiding (lookup)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Control.Monad.Writer.Lazy as W

data ReverseIxMap i {- index -} k {- key -} v {- value -} =
  ReverseIxMap !(Map k v) !(Map i k)

empty :: ReverseIxMap i k v
empty = ReverseIxMap Map.empty Map.empty

getForward :: ReverseIxMap i k v -> Map k v
getForward (ReverseIxMap f _) = f

getBackward :: ReverseIxMap i k v -> Map i k
getBackward (ReverseIxMap _ b) = b

fromList :: (Ord i, Ord k, IsPair v i a) => [(k,v)] -> ReverseIxMap i k v
fromList xs = foldr (\(k,v) m -> insert k v m) empty xs

class IsPair v i a | v -> i a, i a -> v where
  toPair :: v -> (i,a)
  fromPair :: (i,a) -> v

viewIx :: IsPair v i a => v -> i
viewIx = fst . toPair

lookup :: Ord k => k -> ReverseIxMap i k v -> Maybe v
lookup k (ReverseIxMap f _) = Map.lookup k f

lookupReverse :: Ord i => i -> ReverseIxMap i k v -> Maybe k
lookupReverse i (ReverseIxMap _ b) = Map.lookup i b

insert :: (Ord i, Ord k, IsPair v i a) =>
  k -> v -> ReverseIxMap i k v -> ReverseIxMap i k v
insert k v (ReverseIxMap forward backward) = ReverseIxMap forward' backward'
  where
  i = viewIx v
  conflict = Map.lookup i backward
  forward' = Map.insert k v $ case conflict of
    Nothing -> forward
    Just conflicting -> {-
Suppose we have the following map:
A -> a    a_i -> A
B -> b    b_i -> B
C -> c    c_i -> C
and we insert (B, c) into it.

The naive result from just inserting would be:
A -> a    a_i -> A
B -> c    c_i -> B
C -> c
which has a dangling `C -> c` entry

Here we remove that piece -}
      Map.delete conflicting forward
  backward' = Map.insert i k backward

delete :: (Ord i, Ord k, IsPair v i a) =>
  k -> ReverseIxMap i k v -> ReverseIxMap i k v
delete k rm@ (ReverseIxMap forward backward) =
  case Map.lookup k forward of
      Nothing -> rm
      Just v -> ReverseIxMap
        (Map.delete k forward)
        (Map.delete (viewIx v) backward)

mapMaybe :: (Ord i, Ord k, IsPair v i a) =>
  (v -> Maybe a) -> ReverseIxMap i k v -> ReverseIxMap i k v
mapMaybe f = mapMaybeWithKey (const f)

mapMaybeWithKey :: (Ord i, Ord k, IsPair v i a) =>
  (k -> v -> Maybe a) -> ReverseIxMap i k v -> ReverseIxMap i k v
mapMaybeWithKey f (ReverseIxMap forward backward) =
  ReverseIxMap forward' backward'
  where
  (forward', deletions) = W.runWriter $ flip Map.traverseMaybeWithKey forward $
    \key value -> let i = viewIx value in
      case f key value of
        Nothing -> W.tell (Set.singleton i) >> pure Nothing
        Just a -> pure $ Just $ fromPair (i,a)
  backward' = Map.withoutKeys backward deletions

foldrWithKey :: (k -> a -> b -> b) -> b -> ReverseIxMap i k a -> b
foldrWithKey f z (ReverseIxMap forward _) = Map.foldrWithKey f z forward
