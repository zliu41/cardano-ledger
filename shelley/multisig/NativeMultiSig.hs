{-# LANGUAGE NamedFieldPuns #-}
module NativeMultiSig where

import qualified Data.Set as Set
import           Data.Set (Set)
import           Data.Word (Word)
import           Data.ByteString (ByteString)

-- | A simple language for expressing conditions under which it is valid to
-- withdraw from a normal UTxO payment address or to use a stake address.
--
-- The primary use case is for expressing multi-signature payment addresses
-- and multi-signature stake addresses.
--
-- It also includes three primitives:
--
-- * Signatures: the transaction must be signed by the given key;
-- * Hash locks: the pre-image of a hash must be supplied to unlock;
-- * Time locks: that the time (slot) must be before or after some given time.
--
-- These can be combined arbitrarily using logical operations:
--
-- * multi-way \"and\";
-- * multi-way \"or\";
-- * multi-way \"N of M\".
--
-- This makes it easy to express multi-signature addresses, but also allows
-- expressing validity conditions needed for locking funds used with lightning.
--
data Term =

       -- | Require the redeeming transaction be witnessed by the spending key
       --   corresponding to the given verification key hash.
       RequireSignature   VKeyHash

       -- | Require the preimage of the hash to be supplied as part of the
       --   redeeming witness.
     | RequirePreimage    Hash

       -- | Require that the redeeming transaction declare itself to have a
       --   validity window that starts strictly after the given slot number.
     | RequireTimeAfter   SlotNo

       -- | Require that the redeeming transaction declare itself to have a
       --   validity window that ends strictly before the given slot number.
     | RequireTimeBefore  SlotNo

       -- | Require all the sub-terms to be satisfied.
     | RequireAllOf      [Term]

       -- | Require any one of the sub-terms to be satisfied.
     | RequireAnyOf      [Term]

       -- | Require M of the given sub-terms to be satisfied.
     | RequireMOf    Int [Term]
  deriving Show

data VKeyHash = VKeyHash Int -- dummy representation
  deriving (Eq, Ord, Show)

newtype Hash = Hash ByteString
  deriving (Eq, Ord, Show)

newtype SlotNo = SlotNo Word
  deriving (Eq, Ord, Show)


data Env = Env {

       -- | The set of verification key hashes corresponding to the pubkey
       --   signatures in the transaction witnesses. These signatures must
       --   already have been checked.
       envTxSigVKeyHashes :: Set VKeyHash,

       -- | A set of hashes of supplied pre-images, needed to verify any
       -- 'RequirePreimage' test.
       envHashPreimages   :: Set Hash,

       -- | The inclusive lower bound of the tx validity window, if any.
       envTxValidFromSlot :: Maybe SlotNo,

       -- | The inclusive upper bound of the tx validity window, if any.
       envTxValidToSlot   :: Maybe SlotNo
     }

eval :: Env -> Term -> Bool
eval Env{envTxSigVKeyHashes} (RequireSignature vkh) =
    vkh `Set.member` envTxSigVKeyHashes

eval Env{envHashPreimages} (RequirePreimage h) =
    h `Set.member` envHashPreimages

eval Env{envTxValidToSlot = Just t} (RequireTimeBefore t') =
    t < t'

eval Env{envTxValidToSlot = Nothing} (RequireTimeBefore _) =
    False

eval Env{envTxValidFromSlot = Just t} (RequireTimeAfter t') =
    t > t'

eval Env{envTxValidFromSlot = Nothing} (RequireTimeAfter _) =
    False

eval env (RequireAllOf ts) =
    all (eval env) ts

eval env (RequireAnyOf ts) =
    any (eval env) ts

eval env (RequireMOf m ts) =
    mofn m (eval env) ts
  where
    mofn m f xs = sum [ if f x then 1 else 0 | x <- xs ] >= m


mkVKeyHashSet :: [Int] -> Env
mkVKeyHashSet ns =
    Env {
      envTxSigVKeyHashes = Set.fromList (map VKeyHash ns),
      envHashPreimages   = Set.empty,
      envTxValidFromSlot = Nothing,
      envTxValidToSlot   = Nothing
    }

verifyCases :: Term -> [(Env, Bool)] -> Bool
verifyCases term cases =
    and [ eval env term == expected
        | (env, expected) <- cases]

example1Of2 =
  RequireAnyOf [RequireSignature (VKeyHash 1), RequireSignature (VKeyHash 2)]

example1Of2_verify =
    verifyCases
      example1Of2
      [ (mkVKeyHashSet [],    False)
      , (mkVKeyHashSet [1],   True)
      , (mkVKeyHashSet [2],   True)
      , (mkVKeyHashSet [1,2], True)
      , (mkVKeyHashSet [3],   False)
      ]

example2Of2 =
  RequireAllOf [RequireSignature (VKeyHash 1), RequireSignature (VKeyHash 2)]

example2Of2_verify =
    verifyCases
      example2Of2
      [ (mkVKeyHashSet [],      False)
      , (mkVKeyHashSet [1],     False)
      , (mkVKeyHashSet [2],     False)
      , (mkVKeyHashSet [1,2],   True)
      , (mkVKeyHashSet [3],     False)
      , (mkVKeyHashSet [1,2,3], True)
      ]

exampleNestedOrAnd =
  RequireAnyOf [ RequireAllOf [ RequireSignature (VKeyHash 1)
                              , RequireSignature (VKeyHash 2)
                              ]
               , RequireAllOf [ RequireSignature (VKeyHash 3)
                              , RequireSignature (VKeyHash 4)
                              ]
               ]

exampleNestedAndOr =
  RequireAllOf [ RequireAnyOf [ RequireSignature (VKeyHash 1)
                              , RequireSignature (VKeyHash 2)
                              ]
               , RequireAnyOf [ RequireSignature (VKeyHash 3)
                              , RequireSignature (VKeyHash 4)
                              ]
               ]

exampleNestedOrAnd_verify =
    verifyCases
      exampleNestedOrAnd
      [ (keysset, expected)
      | has1 <- [False, True]
      , has2 <- [False, True]
      , has3 <- [False, True]
      , has4 <- [False, True]
      , has5 <- [False, True]
      , let keysset = mkVKeyHashSet $ [ 1 | has1 ]
                                   ++ [ 2 | has2 ]
                                   ++ [ 3 | has3 ]
                                   ++ [ 4 | has4 ]
                                   ++ [ 5 | has5 ]
            expected = (has1 && has2)
                    || (has3 && has4)
      ]

exampleNestedAndOr_verify =
    verifyCases
      exampleNestedAndOr
      [ (keysset, expected)
      | has1 <- [False, True]
      , has2 <- [False, True]
      , has3 <- [False, True]
      , has4 <- [False, True]
      , has5 <- [False, True]
      , let keysset = mkVKeyHashSet $ [ 1 | has1 ]
                                   ++ [ 2 | has2 ]
                                   ++ [ 3 | has3 ]
                                   ++ [ 4 | has4 ]
                                   ++ [ 5 | has5 ]
            expected = (has1 || has2)
                    && (has3 || has4)
      ]

