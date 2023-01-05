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
module SwappableDataType
  ( ownershipSwapCheck
  , priceUpdateCheck
  , priceUpdateWithTimeCheck
  , switchStates
  , successfulAuction
  , proveOwnership
  , SwappableData (..)
  , PayToData (..)
  , PaymentData (..)
  , TimeData (..)
  , checkValidTimeLock
  , ADAIncData (..)
  , MakeOfferData (..)
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Plutus.V2.Ledger.Api as PlutusV2
import           AuctionDataType      ( AuctionData, aSellerPkh, aSellerSc, aLockStart, aLockEnd )
{- |
  Author   : The Ancient Kraken
  Copyright: 2022
-}
-------------------------------------------------------------------------------
-- | Swappable Data Object
-------------------------------------------------------------------------------
data SwappableData = SwappableData
  { sPkh   :: PlutusV2.PubKeyHash
  -- ^ A payment public key hash.
  , sSc    :: PlutusV2.PubKeyHash
  -- ^ A staking credential.
  , sPid   :: PlutusV2.CurrencySymbol
  -- ^ flatrate payment policy id
  , sTkn   :: PlutusV2.TokenName
  -- ^ flatrate payment token name
  , sAmt   :: Integer
  -- ^ flaterate payment token amount
  , sSlip  :: Integer
  -- ^ slippage for order book swaps
  , sStart :: Integer
  -- ^ starting time
  , sEnd   :: Integer
  -- ^ ending time
  }
PlutusTx.unstableMakeIsData ''SwappableData

instance Eq SwappableData where
  {-# INLINABLE (==) #-}
  a == b = ( sPkh   a == sPkh   b ) &&
           ( sSc    a == sSc    b ) &&
           ( sPid   a == sPid   b ) &&
           ( sTkn   a == sTkn   b ) &&
           ( sAmt   a == sAmt   b ) &&
           ( sSlip  a == sSlip  b ) &&
           ( sStart a == sStart b ) &&
           ( sEnd   a == sEnd   b )

-- a is old; b is new
ownershipSwapCheck :: SwappableData -> SwappableData -> Bool
ownershipSwapCheck a b = ( sPkh   a /= sPkh   b ) &&
                         ( sStart a == sStart b ) &&
                         ( sEnd   a == sEnd   b )

-- update price and time
priceUpdateWithTimeCheck :: SwappableData -> SwappableData -> Bool
priceUpdateWithTimeCheck a b =  ( sPkh   a == sPkh   b ) &&
                                ( sSc    a == sSc    b ) &&
                                ( sStart a <= sStart b ) && -- can only increase or remain constant
                                ( sStart b <= sEnd   b ) && -- must be less than or equal to end
                                ( sEnd   a <= sEnd   b )    -- can only increase or remain constant

-- update price only
priceUpdateCheck :: SwappableData -> SwappableData -> Bool
priceUpdateCheck a b = ( sPkh   a == sPkh   b ) &&
                       ( sSc    a == sSc    b ) &&
                       ( sStart a == sStart b ) && -- can only remain constant
                       ( sEnd   a == sEnd   b )    -- can only remain constant

switchStates :: AuctionData -> SwappableData -> Bool
switchStates a b = ( aSellerPkh a == sPkh   b ) &&
                   ( aSellerSc  a == sSc    b ) &&
                   ( aLockStart a == sStart b ) &&
                   ( aLockEnd   a == sEnd   b )

successfulAuction :: AuctionData -> SwappableData -> Bool
successfulAuction a b = ( aSellerPkh a /= sPkh   b ) &&
                        ( aLockStart a == sStart b ) &&
                        ( aLockEnd   a == sEnd   b )
-------------------------------------------------------------------------------
-- | Time Data Object
-------------------------------------------------------------------------------
data TimeData = TimeData
  { tStart :: Integer
  -- ^ starting time
  , tEnd   :: Integer
  -- ^ ending time
  }
PlutusTx.unstableMakeIsData ''TimeData
-- old == new
instance Eq TimeData where
  {-# INLINABLE (==) #-}
  a == b = ( tStart a == tStart b ) &&
           ( tEnd   a == tEnd   b )
-- a is old; b is new
checkValidTimeLock :: TimeData -> TimeData -> Bool
checkValidTimeLock a b =  ( tStart a <= tStart b ) && -- can only increase or remain constant
                          ( tStart b <= tEnd   b ) && -- must be less than or equal to end
                          ( tEnd   a <= tEnd   b )    -- can only increase or remain constant
-------------------------------------------------------------------------------
-- | Payment Data
-------------------------------------------------------------------------------
data PaymentData = PaymentData
  { pPid   :: PlutusV2.CurrencySymbol
  -- ^ flatrate payment policy id
  , pTkn   :: PlutusV2.TokenName
  -- ^ flatrate payment token name
  , pAmt   :: Integer
  -- ^ flaterate payment token amount
  }
PlutusTx.unstableMakeIsData ''PaymentData
-------------------------------------------------------------------------------
-- | Pay To Data
-------------------------------------------------------------------------------
data PayToData = PayToData
  { ptPkh :: PlutusV2.PubKeyHash
  -- ^ pay to this public key hash
  , ptSc  :: PlutusV2.PubKeyHash
  -- ^ pay to this stake key
  }
PlutusTx.unstableMakeIsData ''PayToData

instance Eq PayToData where
  {-# INLINABLE (==) #-}
  a == b = ( ptPkh a == ptPkh b ) &&
           ( ptSc  a == ptSc  b )

proveOwnership :: PayToData -> SwappableData -> Bool
proveOwnership a b = ( ptPkh a == sPkh b ) && 
                     ( ptSc  a == sSc  b )
-------------------------------------------------------------------------------
-- | ADA Increase Data Object
-------------------------------------------------------------------------------
data ADAIncData = ADAIncData
  { adaInc  :: Integer
  -- ^ The increase in the required minimum lovelace .
  }
PlutusTx.unstableMakeIsData ''ADAIncData
-------------------------------------------------------------------------------
-- | Make Offer Data Object
-------------------------------------------------------------------------------
data MakeOfferData = MakeOfferData
  { moTx    :: PlutusV2.BuiltinByteString
  -- ^ The tx hash of the other utxo being swapped.
  , moIdx   :: Integer
  -- ^ The index of the tx hash.
  }
PlutusTx.unstableMakeIsData ''MakeOfferData