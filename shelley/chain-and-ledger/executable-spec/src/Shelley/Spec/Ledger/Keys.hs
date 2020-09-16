{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Shelley.Spec.Ledger.Keys
  ( module Shelley.Spec.Ledger.Data.KeysData,
    asWitness,

    -- * DSIGN
    signedDSIGN,
    verifySignedDSIGN,

    -- * Key hashes
    hashKey,

    -- * Re-exports from cardano-crypto-class
    DSIGN.decodeSignedDSIGN,
    DSIGN.encodeSignedDSIGN,
    Hash.hashWithSerialiser,
    KES.decodeSignedKES,
    KES.decodeVerKeyKES,
    KES.encodeSignedKES,
    KES.encodeVerKeyKES,
    KES.signedKES,
    KES.updateKES,
    KES.verifyKES,
    KES.verifySignedKES,
    VRF.decodeVerKeyVRF,
    VRF.encodeVerKeyVRF,
    VRF.hashVerKeyVRF,
    VRF.verifyVRF,
  )
where

import Shelley.Spec.Ledger.Data.KeysData
import qualified Cardano.Crypto.DSIGN as DSIGN
import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Crypto.KES as KES
import qualified Cardano.Crypto.VRF as VRF
import Cardano.Ledger.Crypto (DSIGN)
import Cardano.Ledger.Era

-- | Use a key as a witness.
--
--   This is the most common coercion between key roles, because most keys can
--   be used as witnesses to some types of transaction. As such, we provide an
--   explicit coercion for it.
asWitness ::
  (HasKeyRole a) =>
  a r era ->
  a 'Witness era
asWitness = coerceKeyRole


-- | Produce a digital signature
signedDSIGN ::
  (Era era, DSIGN.Signable (DSIGN (Crypto era)) a) =>
  DSIGN.SignKeyDSIGN (DSIGN (Crypto era)) ->
  a ->
  SignedDSIGN era a
signedDSIGN key a = DSIGN.signedDSIGN () a key

-- | Verify a digital signature
verifySignedDSIGN ::
  (Era era, DSIGN.Signable (DSIGN (Crypto era)) a) =>
  VKey kd era ->
  a ->
  SignedDSIGN era a ->
  Bool
verifySignedDSIGN (VKey vk) vd sigDSIGN =
  either (const False) (const True) $ DSIGN.verifySignedDSIGN () vk vd sigDSIGN

--------------------------------------------------------------------------------
-- Key Hashes
--------------------------------------------------------------------------------

-- | Hash a given public key
hashKey ::
  ( Era era
  ) =>
  VKey kd era ->
  KeyHash kd era
hashKey (VKey vk) = KeyHash $ DSIGN.hashVerKeyDSIGN vk
