{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}

module Shelley.Spec.Ledger.MetaData
  ( module Shelley.Spec.Ledger.Data.MetaDataData,
    hashMetaData,
  )
where

import Shelley.Spec.Ledger.Data.MetaDataData
import Cardano.Ledger.Era (Era)
import Cardano.Binary
  ( ToCBOR (toCBOR),
  )
import Shelley.Spec.Ledger.Keys (hashWithSerialiser)

hashMetaData ::
  Era era =>
  MetaData ->
  MetaDataHash era
hashMetaData = MetaDataHash . hashWithSerialiser toCBOR
