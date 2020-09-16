{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Shelley.Spec.Ledger.Data.OCertData
  ( OCert (..),
    OCertEnv (..),
    OCertSignable (..),
    KESPeriod (..),
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..), toCBOR)
import qualified Cardano.Crypto.DSIGN as DSIGN
import qualified Cardano.Crypto.KES as KES
import Cardano.Crypto.Util (SignableRepresentation (..))
import Cardano.Ledger.Crypto (KES)
import Cardano.Ledger.Era
import Cardano.Prelude (NoUnexpectedThunks (..))
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Builder.Extra as BS
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Quiet
import Shelley.Spec.Ledger.Keys
  ( KeyHash,
    KeyRole (..),
    SignedDSIGN,
    VerKeyKES,
    decodeSignedDSIGN,
    decodeVerKeyKES,
    encodeSignedDSIGN,
    encodeVerKeyKES,
  )
import Shelley.Spec.Ledger.Serialization
  ( CBORGroup (..),
    FromCBORGroup (..),
    ToCBORGroup (..),
    runByteBuilder,
  )

data OCertEnv era = OCertEnv
  { ocertEnvStPools :: Set (KeyHash 'StakePool era),
    ocertEnvGenDelegs :: Set (KeyHash 'GenesisDelegate era)
  }
  deriving (Show, Eq)

newtype KESPeriod = KESPeriod {unKESPeriod :: Word}
  deriving (Eq, Generic, Ord, NoUnexpectedThunks, FromCBOR, ToCBOR)
  deriving (Show) via Quiet KESPeriod

data OCert era = OCert
  { -- | The operational hot key
    ocertVkHot :: !(VerKeyKES era),
    -- | counter
    ocertN :: !Word64,
    -- | Start of key evolving signature period
    ocertKESPeriod :: !KESPeriod,
    -- | Signature of block operational certificate content
    ocertSigma :: !(SignedDSIGN era (OCertSignable era))
  }
  deriving (Generic)
  deriving (ToCBOR) via (CBORGroup (OCert era))

deriving instance Era era => Eq (OCert era)

deriving instance Era era => Show (OCert era)

instance Era era => NoUnexpectedThunks (OCert era)

instance
  (Era era) =>
  ToCBORGroup (OCert era)
  where
  toCBORGroup ocert =
    encodeVerKeyKES (ocertVkHot ocert)
      <> toCBOR (ocertN ocert)
      <> toCBOR (ocertKESPeriod ocert)
      <> encodeSignedDSIGN (ocertSigma ocert)
  encodedGroupSizeExpr size proxy =
    KES.encodedVerKeyKESSizeExpr (ocertVkHot <$> proxy)
      + encodedSizeExpr size ((toWord . ocertN) <$> proxy)
      + encodedSizeExpr size ((\(KESPeriod p) -> p) . ocertKESPeriod <$> proxy)
      + DSIGN.encodedSigDSIGNSizeExpr (((\(DSIGN.SignedDSIGN sig) -> sig) . ocertSigma) <$> proxy)
    where
      toWord :: Word64 -> Word
      toWord = fromIntegral

  listLen _ = 4
  listLenBound _ = 4

instance
  (Era era) =>
  FromCBORGroup (OCert era)
  where
  fromCBORGroup =
    OCert
      <$> decodeVerKeyKES
      <*> fromCBOR
      <*> fromCBOR
      <*> decodeSignedDSIGN

-- | Signable part of an operational certificate
data OCertSignable era
  = OCertSignable !(VerKeyKES era) !Word64 !KESPeriod

instance
  forall era.
  Era era =>
  SignableRepresentation (OCertSignable era)
  where
  getSignableRepresentation (OCertSignable vk counter period) =
    runByteBuilder
      ( fromIntegral $
          KES.sizeVerKeyKES (Proxy @(KES (Crypto era)))
            + 8
            + 8
      )
      $ BS.byteStringCopy (KES.rawSerialiseVerKeyKES vk)
        <> BS.word64BE counter
        <> BS.word64BE (fromIntegral $ unKESPeriod period)
