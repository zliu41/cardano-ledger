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

module Shelley.Spec.Ledger.Value
 where

import           Cardano.Binary (ToCBOR, FromCBOR, toCBOR, fromCBOR, encodeListLen)
import           Cardano.Prelude (NoUnexpectedThunks(..), NFData ())
import           Shelley.Spec.Ledger.Serialization (decodeRecordNamed)

import           Shelley.Spec.Ledger.Coin (Coin (..))
import           GHC.Generics (Generic)
import           Data.Map.Strict(Map)
import qualified Data.Map as Map

import           Data.ByteString (ByteString) -- TODO is this the right Bytestring
import           Shelley.Spec.Ledger.Scripts
import           Shelley.Spec.Ledger.Val
import           Cardano.Ledger.Era

-- ======================================================================
-- Multi Assests
--
-- A Value is a map from 'PolicyID's to a quantity of assets with this policy.
-- This map implements a finitely supported functions ovr PolicyId.
-- A PolicyID is not stored in the Map, then its quantity is assumed to be 0.
--
-- Operations on assets are usually implemented 'pointwise'. That is,
-- we apply the operation to the quantities for each asset in turn. So
-- when we add two 'Value's the resulting 'Value' has, for each asset,
-- the sum of the quantities of /that particular/ asset in the argument
-- 'Value'. The effect of this is that the assets in the 'Value' are "independent",
-- and are operated on separately.
--
-- We can think of 'Value' as a vector space whose dimensions are
-- assets. At the moment there is only a single asset type (Ada), so 'Value'
-- contains one-dimensional vectors. When asset-creating transactions are
-- implemented, this will change and the definition of 'Value' will change to a
-- 'Map Asset Int', effectively a vector with infinitely many dimensions whose
-- non-zero values are recorded in the map.
--
-- To create a value of 'Value', we need to specifiy an asset policy. This can be done
-- using 'Ledger.Ada.adaValueOf'. To get the ada dimension of 'Value' we use
-- 'Ledger.Ada.fromValue'. Plutus contract authors will be able to define modules
-- similar to 'Ledger.Ada' for their own assets.
-- ======================================================================================

-- | Quantity
newtype Quantity = Quantity {unInt :: Integer}
  deriving (Show, Eq, Generic, ToCBOR, FromCBOR, Ord, Integral, Real, Num, Enum, NoUnexpectedThunks, NFData)

deriving instance Val Quantity

-- | Asset ID
newtype AssetID = AssetID {assetID :: ByteString}
  deriving (Show, Eq, ToCBOR, FromCBOR, Ord, NoUnexpectedThunks, NFData)

-- | Policy ID
newtype PolicyID era = PolicyID {policyID :: ScriptHash era}
  deriving (Show, Eq, ToCBOR, FromCBOR, Ord, NoUnexpectedThunks, NFData)

-- | The Value representing MultiAssets
data Value era = Value Coin (Map (PolicyID era) (Map AssetID Quantity))
  deriving (Show, Generic)

-- =============================================
-- Operations, and class instances on Value

class Default f t where
  apply:: Ord k => f k t -> k -> t

instance NFData (Value era)
instance NoUnexpectedThunks (Value era)

instance Val t => Default Map t where
   apply mp k = case Map.lookup k mp of { Just t -> t; Nothing -> vzero }

instance Eq (Value era) where
    (==) (Value c v) (Value c1 v1) = (voper Equal c c1) && (voper Equal v v1)

instance Semigroup (Value era) where
    (<>) = vplus

instance Monoid (Value era) where
    mempty  = vzero
    mappend = (<>)

instance Val (Value era) where
  vzero = Value (Coin 0) vzero
  vplus (Value c1 v1) (Value c2 v2) = Value (vplus c1 c2) (vplus v1 v2)
  vnegate (Value c1 v1) = Value (vnegate c1) (vnegate v1)
  scalev s (Value c1 v1) = Value (scalev s c1) (scalev s v1)
  voper op (Value c1 v1) (Value c2 v2) = (voper op c1 c2) && (voper op v1 v2)
  visZero (Value c1 v1) = (visZero c1) && (visZero v1)
  vcoin (Value c1 _) = c1
  vinject c1 = Value c1 vzero
  vsize (Value _ v) = foldr accum uint v where                     -- add uint for the Coin portion in this size calculation
    accum u ans = foldr accumIns (ans + addrHashLen) u where       -- add addrHashLen for each Policy ID
      accumIns _ ans1 = ans1 + assetIdLen + uint                   -- add assetIdLen and uint for each asset of that Policy ID

-- ============================================
-- Constants needed to compute size and size-scaling operation

-- address hash length is always same as Policy ID length
addrHashLen :: Integer
addrHashLen = 28

smallArray :: Integer
smallArray = 1

hashLen :: Integer
hashLen = 32

assetIdLen :: Integer
assetIdLen = 32

uint :: Integer
uint = 5

hashObj :: Integer
hashObj = 2 + hashLen

addrHeader :: Integer
addrHeader = 1

address :: Integer
address = 2 + addrHeader + 2 * addrHashLen

-- input size
inputSize :: Integer
inputSize = smallArray + uint + hashObj

-- size of output not including the Val (compute that part with vsize later)
outputSizeWithoutVal :: Integer
outputSizeWithoutVal = smallArray + address

-- size of the UTxO entry (ie the space the scaled minUTxOValue deposit pays)
utxoEntrySizeWithoutVal :: Integer
utxoEntrySizeWithoutVal = inputSize + outputSizeWithoutVal

-- This scaling function is right for UTxO, not EUTxO
scaledMinDeposit :: (Val v) => v -> Coin -> Coin
scaledMinDeposit v (Coin mv)
  | vinject (vcoin v) == v = Coin mv  -- without non-Coin assets, scaled deposit should be exactly minUTxOValue
  | otherwise              = Coin $ fst $ quotRem (mv * (utxoEntrySizeWithoutVal + uint)) (utxoEntrySizeWithoutVal + vsize v) -- round down

-- compare the outputs as Values (finitely supported functions)
-- ada must be greater than scaled min value deposit
-- rest of tokens must be greater than 0
-- by :
-- outputsTooSmall = [out | out@(TxOut _ vl) <- outputs, (voper Gt) (vinject $ scaleVl vl minUTxOValue) vl]

-- =============================================================
-- Operations needed for Tests

class Val t => ValTest t where
   vsplit :: t -> Integer -> ([t], Coin)
   vmodify:: Monad m => (Coin -> m Coin) -> t -> m t

instance ValTest Coin where
  vsplit (Coin n) 0 = ([], Coin n)
  vsplit (Coin n) m  -- TODO fix this?
    | m <= 0 = error "must split coins into positive parts"
    | otherwise = (take (fromIntegral m) (repeat (Coin(n `div` m))), Coin (n `rem` m))
  vmodify f coin = f coin

instance ValTest (Value era) where
  vsplit (Value coin _) 0 = ([], coin) -- The sum invariant may not hold, but no other way to split into 0 groups
  vsplit (Value coin assets) m = (zipWith Value coins maps,remainder)
    where
      maps = assets : (take (fromIntegral $ m - 1) (repeat vzero))
      (coins, remainder) = vsplit coin m
  vmodify f (Value coin assets) = do { coin2 <- f coin; pure(Value coin2 assets)}

-- CBOR


instance
  (Era era)
  => ToCBOR (Value era)
 where
   toCBOR (Value c v) =
           encodeListLen 2
           <> toCBOR c
           <> toCBOR v

-- filter out 0s right at deserialization
-- TODO
instance
  (Era era)
  => FromCBOR (Value era)
 where
  fromCBOR = do
    decodeRecordNamed "Value" (const 2) $ do
      c <- fromCBOR
      v <- fromCBOR
      pure $ Value c v
