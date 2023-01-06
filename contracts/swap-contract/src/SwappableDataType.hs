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
  , TimeData (..)
  , checkValidTimeLock
  , checkValidTimeData
  , ADAIncData (..)
  , MakeOfferData (..)
  , SpecificToken (..)
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Plutus.V2.Ledger.Api as PlutusV2
{- |
  Author   : The Ancient Kraken
  Copyright: 2022
-}
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
-------------------------------------------------------------------------------
-- | Payment Data
-------------------------------------------------------------------------------
data PaymentData = PaymentData
  { pPid :: PlutusV2.CurrencySymbol
  -- ^ flatrate payment policy id
  , pTkn :: PlutusV2.TokenName
  -- ^ flatrate payment token name
  , pAmt :: Integer
  -- ^ flaterate payment token amount
  , pAny :: Integer
  -- ^ A flag that allows any token to from a pid to be used.
  }
PlutusTx.unstableMakeIsData ''PaymentData
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

-- check a time data
checkValidTimeData :: TimeData -> Bool
checkValidTimeData a = ( tStart a <= tEnd a )
-------------------------------------------------------------------------------
-- | ADA Increase Data Object
-------------------------------------------------------------------------------
data ADAIncData = ADAIncData 
  { adaInc :: Integer
  -- ^ An increase to the required minimum ADA.
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
-------------------------------------------------------------------------------
-- | Specific Token Data Object
-------------------------------------------------------------------------------
data SpecificToken = SpecificToken
  { sTkn :: PlutusV2.TokenName
  -- ^^ The specific token name being used in the flatrate swap.
  }
PlutusTx.unstableMakeIsData ''SpecificToken