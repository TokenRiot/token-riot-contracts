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
module AuctionDataType
  ( resetAuction
  , auctionBidCheck
  , AuctionData (..)
  , BidData (..)
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Plutus.V2.Ledger.Api as PlutusV2
{- |
  Author   : The Ancient Kraken
  Copyright: 2022
-}
-------------------------------------------------------------------------------
-- | Time Locking Data Object
-------------------------------------------------------------------------------
data AuctionData = AuctionData
  { aSellerPkh :: PlutusV2.PubKeyHash
  -- ^ The Seller's public key hash.
  , aSellerSc  :: PlutusV2.PubKeyHash
  -- ^ The Seller's staking credential hash.
  , aPid :: PlutusV2.CurrencySymbol
  -- ^ The selected payment policy id.
  , aTkn :: PlutusV2.TokenName
  -- ^ The selected payment token.
  , aPrice     :: Integer
  -- ^ The amount of the token the Seller will receive at this moment.
  , aMinimum   :: Integer
  -- ^ The minimum amount of the token the Seller, default should be 0.
  , aStartTime :: Integer
  -- ^ The starting time measured as nanoseconds from unix time.
  , aEndTime   :: Integer
  -- ^ The ending time measured as nanoseconds.
  , aBidderPkh :: PlutusV2.PubKeyHash
  -- ^ The Bidder's public key hash.
  , aBidderSc  :: PlutusV2.PubKeyHash
  -- ^ The Bidder's staking credential hash.
  , aLockStart :: Integer
  -- ^ The global starting time lock from the swap state.
  , aLockEnd   :: Integer
  -- ^ The global ending time lock from the swap state.
  }
PlutusTx.unstableMakeIsData ''AuctionData

-- old == new
auctionBidCheck :: AuctionData -> AuctionData -> Bool
auctionBidCheck a b = ( aSellerPkh a == aSellerPkh b ) &&
                      ( aSellerSc  a == aSellerSc  b ) &&
                      ( aPid       a == aPid       b ) &&
                      ( aTkn       a == aTkn       b ) &&
                      ( aPrice     a  < aPrice     b ) &&
                      ( aPrice     b  > aMinimum   a ) &&
                      ( aMinimum   a == aMinimum   b ) &&
                      ( aStartTime a == aStartTime b ) &&
                      ( aEndTime   a == aEndTime   b ) &&
                      ( aBidderPkh a /= aBidderPkh b ) &&
                      ( aLockStart a == aLockStart b ) &&
                      ( aLockEnd   a == aLockEnd   b )

resetAuction :: AuctionData -> AuctionData -> Bool
resetAuction a b = ( aSellerPkh a == aSellerPkh b ) &&
                   ( aSellerSc  a == aSellerSc  b ) &&
                   ( aLockStart a == aLockStart b ) &&
                   ( aLockEnd   a == aLockEnd   b )

-------------------------------------------------------------------------------
-- | Bid Data Object
-------------------------------------------------------------------------------
data BidData = BidData
  { bAmt :: Integer
  -- ^ bid amount
  }
PlutusTx.unstableMakeIsData ''BidData