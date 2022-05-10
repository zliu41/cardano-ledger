{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Cardano.Ledger.Babbage.Tx
  ( module X,
    TxBody (..),
  )
where

import Cardano.Ledger.Alonzo.Tx as X hiding (TxBody (..))
import Cardano.Ledger.Babbage.TxBody (TxBody (..))

