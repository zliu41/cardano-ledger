{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Pre-generated items to use in traces.
--
--   Functions in this module make specific assumptions about the sets of keys
--   involved, and thus cannot be used as generic generators.
module Test.Shelley.Spec.Ledger.Generator.Presets
  ( coreNodeKeys,
    keySpace,
    genEnv,
    genesisDelegs0,
    someKeyPairs,
    keyPairs,
  )
where

import qualified Cardano.Ledger.Crypto as CC (Crypto, KES)
import qualified Cardano.Ledger.Era as Era (Crypto)
import Cardano.Crypto.KES.Class (KESSignAlgorithm)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word64)
import Shelley.Spec.Ledger.Keys
  ( GenDelegPair (..),
    KeyHash,
    KeyPair (..),
    KeyRole (..),
    coerceKeyRole,
    hashKey,
    hashVerKeyVRF,
  )
import Shelley.Spec.Ledger.LedgerState
  ( KeyPairs,
  )
import Shelley.Spec.Ledger.OCert (KESPeriod (..))
import Test.QuickCheck (Gen)
import qualified Test.QuickCheck as QC
import Test.Shelley.Spec.Ledger.Generator.Constants
  ( Constants (..),
    defaultConstants,
  )
import Test.Shelley.Spec.Ledger.Generator.Core
import Test.Shelley.Spec.Ledger.Generator.ScriptClass
  ( ScriptClass (..),
    combinedScripts,
    keyPairs,
  )
import Test.Shelley.Spec.Ledger.Utils
  ( maxKESIterations,
    mkKESKeyPair,
    mkVRFKeyPair,
    slotsPerKESIteration,
  )
import Control.Monad (forM)

-- | Example generator environment, consisting of default constants and an
-- corresponding keyspace.
genEnv ::
  (ScriptClass era, KESSignAlgorithm m (CC.KES (Era.Crypto era))) =>
  proxy era ->
  m (GenEnv era)
genEnv _ =
  GenEnv
    <$> keySpace defaultConstants
    <*> pure defaultConstants

-- | Example keyspace for use in generators
keySpace ::
  forall era m.
  (ScriptClass era, KESSignAlgorithm m (CC.KES (Era.Crypto era))) =>
  Constants ->
  m (KeySpace era)
keySpace c =
  KeySpace
    <$> (coreNodeKeys c)
    <*> (genesisDelegates c)
    <*> (stakePoolKeys c)
    <*> pure (keyPairs c)
    <*> pure (combinedScripts @era c)

-- | Select between _lower_ and _upper_ keys from 'keyPairs'
someKeyPairs :: CC.Crypto crypto => Constants -> Int -> Int -> Gen (KeyPairs crypto)
someKeyPairs c lower upper =
  take
    <$> QC.choose (lower, upper)
    <*> QC.shuffle (keyPairs c)

-- Pairs of (genesis key, node keys)
--
-- NOTE: we use a seed range in the [1000...] range
-- to create keys that don't overlap with any of the other generated keys
coreNodeKeys ::
  (CC.Crypto crypto, KESSignAlgorithm m (CC.KES crypto)) =>
  Constants ->
  m [(KeyPair 'Genesis crypto, AllIssuerKeys crypto 'GenesisDelegate)]
coreNodeKeys c@Constants {numCoreNodes} =
  forM [1001 .. 1000 + numCoreNodes] $ \x -> do
    let kp = (toKeyPair . mkGenKey) (x, 0, 0, 0, 0)
    ik <- issuerKeys c 0 x
    return (kp, ik)
  where
    toKeyPair (sk, vk) = KeyPair vk sk

-- Pre-generate a set of keys to use for genesis delegates.
genesisDelegates :: (CC.Crypto crypto, KESSignAlgorithm m (CC.KES crypto)) => Constants -> m [AllIssuerKeys crypto 'GenesisDelegate]
genesisDelegates c =
  mapM (issuerKeys c 10) [0 .. 50]

-- Pre-generate a set of keys to use for stake pools.
stakePoolKeys :: (CC.Crypto crypto, KESSignAlgorithm m (CC.KES crypto)) => Constants -> m [AllIssuerKeys crypto 'StakePool]
stakePoolKeys c =
  mapM (issuerKeys c 10) [0 .. 50]

-- | Generate all keys for any entity which will be issuing blocks.
issuerKeys ::
  (CC.Crypto crypto, KESSignAlgorithm m (CC.KES crypto)) =>
  Constants ->
  -- | Namespace parameter. Can be used to differentiate between different
  --   "types" of issuer.
  Word64 ->
  Word64 ->
  m (AllIssuerKeys crypto r)
issuerKeys Constants {maxSlotTrace} ns x = do
  let (skCold, vkCold) = mkKeyPair (x, 0, 0, 0, ns + 1)
  let iterRange =
            [ 0
              .. ( 1
                     + div
                       maxSlotTrace
                       ( fromIntegral
                           (maxKESIterations * slotsPerKESIteration)
                       )
                 )
            ]
  hotKP <- forM iterRange $ \iter -> do
    kp <- mkKESKeyPair (x, 0, 0, fromIntegral iter, ns + 3)
    return ( KESPeriod (fromIntegral (iter * fromIntegral maxKESIterations)),
             kp
           )
  return AllIssuerKeys
        { cold = KeyPair vkCold skCold,
          hot = hotKP,
          vrf = mkVRFKeyPair (x, 0, 0, 0, ns + 2),
          hk = hashKey vkCold
        }

genesisDelegs0 ::
  (CC.Crypto crypto, KESSignAlgorithm m (CC.KES crypto)) =>
  Constants ->
  m (Map (KeyHash 'Genesis crypto) (GenDelegPair crypto))
genesisDelegs0 c = do
  cnk <- coreNodeKeys c
  return $ Map.fromList
    [ ( hashVKey gkey,
        GenDelegPair
          (coerceKeyRole $ hashVKey (cold pkeys))
          (hashVerKeyVRF . snd . vrf $ pkeys)
      )
      | (gkey, pkeys) <- cnk
    ]
  where
    hashVKey = hashKey . vKey
