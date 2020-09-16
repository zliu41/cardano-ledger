{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}

module Shelley.Spec.Ledger.Scripts
  ( module Shelley.Spec.Ledger.Data.ScriptsData,
    getKeyCombination,
    getKeyCombinations,
    hashAnyScript,
    hashMultiSigScript,
  )
where

import Shelley.Spec.Ledger.Data.ScriptsData
import Cardano.Ledger.Era (Era (..))
import Cardano.Binary
  ( serialize',
  )
import qualified Cardano.Crypto.Hash as Hash
import qualified Data.List as List (concat, concatMap, permutations)
import Shelley.Spec.Ledger.Keys (KeyHash (..), KeyRole (Witness))


-- | Hashes native multi-signature script.
hashMultiSigScript ::
  Era era =>
  MultiSig era ->
  ScriptHash era
hashMultiSigScript =
  ScriptHash
    . Hash.castHash
    . Hash.hashWith (\x -> nativeMultiSigTag <> serialize' x)

hashAnyScript ::
  Era era =>
  Script era ->
  ScriptHash era
hashAnyScript (MultiSigScript msig) = hashMultiSigScript msig

-- | Get one possible combination of keys for multi signature script
getKeyCombination :: Era era => MultiSig era -> [KeyHash 'Witness era]
getKeyCombination (RequireSignature hk) = [hk]
getKeyCombination (RequireAllOf msigs) =
  List.concatMap getKeyCombination msigs
getKeyCombination (RequireAnyOf msigs) =
  case msigs of
    [] -> []
    x : _ -> getKeyCombination x
getKeyCombination (RequireMOf m msigs) =
  List.concatMap getKeyCombination (take m msigs)

-- | Get all valid combinations of keys for given multi signature. This is
-- mainly useful for testing.
getKeyCombinations :: Era era => MultiSig era -> [[KeyHash 'Witness era]]
getKeyCombinations (RequireSignature hk) = [[hk]]
getKeyCombinations (RequireAllOf msigs) =
  [ List.concat $
      List.concatMap getKeyCombinations msigs
  ]
getKeyCombinations (RequireAnyOf msigs) = List.concatMap getKeyCombinations msigs
getKeyCombinations (RequireMOf m msigs) =
  let perms = map (take m) $ List.permutations msigs
   in map (concat . List.concatMap getKeyCombinations) perms
