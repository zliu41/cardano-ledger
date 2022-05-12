{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.Babbage.Tx
  ( module X,
    TxBody (..),
  )
where

import Cardano.Ledger.Alonzo.Tx as X hiding (TxBody (..))
import Cardano.Ledger.Babbage.TxBody (TxBody (..))
