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
  ( PayToData (..)
  , PaymentData (..)
  , defaultPayment
  , TimeData (..)
  , checkValidTimeLock
  , checkValidTimeData
  , ADAIncData (..)
  , MakeOfferData (..)
  , SpecificToken (..)
  , getTokenName
  , OfferFlagData (..)
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Plutus.V2.Ledger.Api as PlutusV2
{- |
  Author   : The Ancient Kraken
  Copyright: 2023
-}
-------------------------------------------------------------------------------
-- | Pay To Data
-- 
-- Holds the data for the owner of some utxo. The ptPkh is the signing key and
-- the combination of ptPkh and ptSc is the address.
--
-- @see: UsefulFuncs.createAddress
-------------------------------------------------------------------------------
data PayToData = PayToData
  { ptPkh :: PlutusV2.PubKeyHash
  -- ^ Pay to this public key hash.
  , ptSc  :: PlutusV2.PubKeyHash
  -- ^ Pay to this stake key.
  }
PlutusTx.unstableMakeIsData ''PayToData

instance Eq PayToData where
  {-# INLINABLE (==) #-}
  a == b = ( ptPkh a == ptPkh b ) &&
           ( ptSc  a == ptSc  b )
-------------------------------------------------------------------------------
-- | Payment Data
--
-- Holds the data for the payment to be accepted for some UTxO. If the pAny flag
-- is zero then the token name to be used inside the tx is pTkn else the token
-- name is supplied in the SpecificToken Data object.
--
-------------------------------------------------------------------------------
data PaymentData = PaymentData
  { pPid :: PlutusV2.CurrencySymbol
  -- ^ The flat rate payment policy id.
  , pTkn :: PlutusV2.TokenName
  -- ^ The flat rate payment token name.
  , pAmt :: Integer
  -- ^ The flat rate payment token amount.
  , pAny :: Integer
  -- ^ A flag that allows any token to from a pid to be used.
  }
PlutusTx.unstableMakeIsData ''PaymentData

instance Eq PaymentData where
  {-# INLINABLE (==) #-}
  a == b = ( pPid a == pPid b ) &&
           ( pTkn a == pTkn b ) &&
           ( pAmt a == pAmt b ) &&
           ( pAny a == pAny b )

-- The default payment is 0 ADA and the any flag set to zero. A unsendable amount.
defaultPayment :: PaymentData
defaultPayment = PaymentData
  { pPid = PlutusV2.CurrencySymbol { PlutusV2.unCurrencySymbol = emptyByteString }
  , pTkn = PlutusV2.TokenName      { PlutusV2.unTokenName      = emptyByteString }
  , pAmt = 0
  , pAny = 0
  }
-------------------------------------------------------------------------------
-- | Time Data Object
--
-- Holds the time information for a UTxO. It is meant to defined some range in
-- time between some point A and B.
--
-- @see: UsefulFuncs.lockBetweenTimeInterval
-------------------------------------------------------------------------------
data TimeData = TimeData
  { tStart :: Integer
  -- ^ The starting unix time.
  , tEnd   :: Integer
  -- ^ The ending unix time.
  }
PlutusTx.unstableMakeIsData ''TimeData

instance Eq TimeData where
  {-# INLINABLE (==) #-}
  a == b = ( tStart a == tStart b ) &&
           ( tEnd   a == tEnd   b )

-- Check if a time data is logically being updated.
checkValidTimeLock :: TimeData -> TimeData -> Bool
checkValidTimeLock a b =  ( tStart a <= tStart b ) && -- can only increase or remain constant
                          ( tStart b <= tEnd   b ) && -- must be less than or equal to end
                          ( tEnd   a <= tEnd   b )    -- can only increase or remain constant

-- Check if a time data has a logical format.
checkValidTimeData :: TimeData -> Bool
checkValidTimeData a = ( tStart a <= tEnd a )
-------------------------------------------------------------------------------
-- | ADA Increase Data Object
--
-- Holds the integer amount of lovelace that will be added to some UTxO. This is
-- useful for transactions where the datum requires more minimum ada. It is supposed
-- to be created by the spender and placed into a redeemer.
--
-------------------------------------------------------------------------------
data ADAIncData = ADAIncData 
  { adaInc :: Integer
  -- ^ An increase to the ADA on a UTxO.
  }
PlutusTx.unstableMakeIsData ''ADAIncData
-------------------------------------------------------------------------------
-- | Make Offer Data Object
--
-- The TxId of the UTxO that an offer belongs too.
--
-- @see: createTxOutRef
-------------------------------------------------------------------------------
data MakeOfferData = MakeOfferData
  { moTx  :: PlutusV2.BuiltinByteString
  -- ^ The tx hash of the other utxo being swapped.
  , moIdx :: Integer
  -- ^ The index of the tx hash.
  }
PlutusTx.unstableMakeIsData ''MakeOfferData
-------------------------------------------------------------------------------
-- | Specific Token Data Object
-------------------------------------------------------------------------------
data SpecificToken = SpecificToken
  { sTkn :: PlutusV2.TokenName
  -- ^^ The specific token name being used in the flatrate swap.
  }
PlutusTx.unstableMakeIsData ''SpecificToken

-- Given a payment data and a specific token return the correct token name.
getTokenName :: PaymentData -> SpecificToken -> PlutusV2.TokenName
getTokenName pay tkn =
  if pAny pay == 0
    then pTkn pay
    else sTkn tkn
-------------------------------------------------------------------------------
-- | Offer Flag Data Object
-------------------------------------------------------------------------------
data OfferFlagData = OfferFlagData
  { oFlag :: Integer
  -- ^ The flag to indicate if the trade should remain in the contract.
  }
PlutusTx.unstableMakeIsData ''OfferFlagData
