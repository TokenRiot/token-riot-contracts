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
  ( IncreaseData (..) 
  , CashierAddressData (..)
  , ServiceFeeData (..)
  , SigningData (..)
  , changeHotKeyOnly
  , lengthCheck
  , StakePoolData (..)
  , ReferenceDatum (..)
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Plutus.V2.Ledger.Api as V2
{- |
  Author   : The Ancient Kraken
  Copyright: 2023
-}
-------------------------------------------------------------------------------
-- | ADA Increase Data Object
--
-- Holds the integer amount of lovelace that will be added to some UTxO. This is
-- useful for transactions where the datum requires more minimum ada. It is supposed
-- to be created by the spender and placed into a redeemer.
-------------------------------------------------------------------------------
data IncreaseData = IncreaseData 
  { idADA :: Integer
  -- ^ An increase to the ADA on a UTxO.
  }
PlutusTx.makeIsDataIndexed ''IncreaseData [('IncreaseData, 0)]
-------------------------------------------------------------------------------
-- | Cashier Payment Info
--
-- Holds the address information for the cash register. This address is designed
-- to auto-roll up payments into the profit address.
-------------------------------------------------------------------------------
data CashierAddressData = CashierAddressData
  { caPkh :: V2.PubKeyHash
  -- ^ Pay to this public key hash
  , caSc  :: V2.PubKeyHash
  -- ^ Pay to this stake key
  }
PlutusTx.makeIsDataIndexed ''CashierAddressData [('CashierAddressData, 0)]

instance Eq CashierAddressData where
  {-# INLINABLE (==) #-}
  a == b = ( caPkh a == caPkh b ) &&
           ( caSc  a == caSc  b )
-------------------------------------------------------------------------------
-- | Fee Payout Info
--
-- The service fee information for the marketplace. The serviceFee is the flat
-- rate fee defined in lovelace while the servicePerc is the percentage fee, 
-- defined as 100/k = n%. This means a servicePerc of 40 is 2.5%. 
-- The cancellationFee is the lovelace required to break a timelock.
-------------------------------------------------------------------------------
data ServiceFeeData = ServiceFeeData
  { servicePerc     :: Integer
  -- ^ The service provider fee percentage
  , serviceFee      :: Integer
  -- ^ Mando service fee
  , cancellationFee :: Integer
  -- ^ The fee to cancel a time lock
  }
PlutusTx.makeIsDataIndexed ''ServiceFeeData [('ServiceFeeData, 0)]

instance Eq ServiceFeeData where
  {-# INLINABLE (==) #-}
  a == b = ( servicePerc     a == servicePerc     b ) &&
           ( serviceFee      a == serviceFee      b ) &&
           ( cancellationFee a == cancellationFee b )
-------------------------------------------------------------------------------
-- | Multisig and Hotkey Information
--
-- The majority of data changes require a valid multisig using the information
-- inside the SigningData. It is a n-out-of-m style signature. The hot key is
-- used for delegation control and cip68 minting.
-------------------------------------------------------------------------------
data SigningData = SigningData
  { mPkhs  :: [V2.PubKeyHash]
  -- ^ List of the multisig public key hashes
  , mThres :: Integer
  -- ^ The number of multsig sigatures required
  , mHot   :: V2.PubKeyHash
  -- ^ The token riot hot key
  }
PlutusTx.makeIsDataIndexed ''SigningData [('SigningData, 0)]

instance Eq SigningData where
  {-# INLINABLE (==) #-}
  a == b = ( mPkhs  a == mPkhs  b ) &&
           ( mThres a == mThres b ) &&
           ( mHot   a == mHot   b )

changeHotKeyOnly :: SigningData -> SigningData -> Bool
changeHotKeyOnly a b = (mPkhs a == mPkhs b) && (mThres a == mThres b)

-- make sure there are no illogical multisigs
lengthCheck :: SigningData -> Bool
lengthCheck msd = lengthCheck' pkhs 0
  where
    pkhs :: [V2.PubKeyHash]
    pkhs = mPkhs msd

    thres :: Integer
    thres = mThres msd

    lengthCheck' :: [V2.PubKeyHash] -> Integer -> Bool
    lengthCheck' []     !counter = counter >= thres
    lengthCheck' (_:xs) !counter =
      if counter >= thres
        then True                          -- there are enough signers
        else lengthCheck' xs (counter + 1) -- loop to the next one
-------------------------------------------------------------------------------
-- | Stake Pool Information
--
-- The poolId is the hash not the pool1 prefixed-key. This is the pool that the
-- stake contract will be delegated too. The staking rewards can only be sent
-- to the reward address.
-------------------------------------------------------------------------------
data StakePoolData = StakePoolData
  { poolId    :: V2.PubKeyHash
  -- ^ The pool where the contract will be staked
  , rewardPkh :: V2.PubKeyHash
  -- ^ The reward public key has for staking
  , rewardSc  :: V2.PubKeyHash
  -- ^ The reward staking credential for staking
  }
PlutusTx.makeIsDataIndexed ''StakePoolData [('StakePoolData, 0)]

instance Eq StakePoolData where
  {-# INLINABLE (==) #-}
  a == b = ( poolId    a == poolId    b ) &&
           ( rewardPkh a == rewardPkh b ) &&
           ( rewardSc  a == rewardSc  b )
-------------------------------------------------------------------------------
-- | Reference Datum
-------------------------------------------------------------------------------
data ReferenceDatum = Reference CashierAddressData ServiceFeeData SigningData StakePoolData
PlutusTx.makeIsDataIndexed ''ReferenceDatum [('Reference, 0)]