{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
-- Options
{-# OPTIONS_GHC -fno-strictness               #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas   #-}
{-# OPTIONS_GHC -fobject-code                 #-}
{-# OPTIONS_GHC -fno-specialise               #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings       #-}
module ReferenceDataType
  ( CashierAddressData (..)
  , ServiceFeeData (..)
  , MultisigData (..)
  , ReferenceDatum (..)
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Plutus.V2.Ledger.Api as V2
-------------------------------------------------------------------------------
-- | Cashier Payment Info
-------------------------------------------------------------------------------
data CashierAddressData = CashierAddressData
  { caPkh :: V2.PubKeyHash
  -- ^ Pay to this public key hash.
  , caSc  :: V2.PubKeyHash
  -- ^ Pay to this stake key.
  }
PlutusTx.unstableMakeIsData ''CashierAddressData

-------------------------------------------------------------------------------
-- | Fee Payout Info
-------------------------------------------------------------------------------
data ServiceFeeData = ServiceFeeData
  { servicePerc :: Integer
  -- ^ The service provider fee percentage
  , serviceFee  :: Integer
  -- ^ Mando service fee
  }
PlutusTx.unstableMakeIsData ''ServiceFeeData

-------------------------------------------------------------------------------
-- | Multisig Information
-------------------------------------------------------------------------------
data MultisigData = MultisigData
  { mPkhs :: [V2.PubKeyHash]
  -- ^ List of the multisig public key hashes
  , mThres :: Integer
  -- ^ The number of multsig sigatures required.
  }
PlutusTx.unstableMakeIsData ''MultisigData

-------------------------------------------------------------------------------
-- | Reference Datum
-------------------------------------------------------------------------------
data ReferenceDatum = Reference CashierAddressData ServiceFeeData MultisigData
PlutusTx.unstableMakeIsData ''ReferenceDatum