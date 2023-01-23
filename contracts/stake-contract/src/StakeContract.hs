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
{-# LANGUAGE NumericUnderscores    #-}
-- Options
{-# OPTIONS_GHC -fno-strictness               #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas   #-}
{-# OPTIONS_GHC -fobject-code                 #-}
{-# OPTIONS_GHC -fno-specialise               #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings       #-}
module StakeContract
  ( stakingPlutusScript
  , stakingScriptShortBs
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import           Cardano.Api.Shelley            ( PlutusScript (..), PlutusScriptV2 )
import           Codec.Serialise                ( serialise )
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.ByteString.Short          as SBS
import qualified PlutusTx.AssocMap              as AM
import qualified Plutus.V1.Ledger.Scripts       as Scripts
import qualified Plutus.V2.Ledger.Api           as PlutusV2
import           Plutus.Script.Utils.V2.Scripts as Utils
import           UsefulFuncs
{-
  Author   : The Ancient Kraken
  Copyright: 2023
-}
-------------------------------------------------------------------------------
-- | The only allowed pool.
-------------------------------------------------------------------------------
poolId :: PlutusV2.PubKeyHash
poolId = PlutusV2.PubKeyHash { PlutusV2.getPubKeyHash = createBuiltinByteString [189, 102, 172, 41, 26, 197, 33, 222, 175, 34, 20, 31, 176, 229, 226, 240, 237, 61, 166, 49, 89, 185, 244, 158, 31, 80, 10, 207] }
-------------------------------------------------------------------------------
-- | The only allowed payout address.
-------------------------------------------------------------------------------
payoutPkh :: PlutusV2.PubKeyHash
payoutPkh = PlutusV2.PubKeyHash { PlutusV2.getPubKeyHash = createBuiltinByteString [66, 23, 239, 18, 207, 227, 97, 45, 43, 20, 117, 103, 227, 20, 151, 188, 12, 10, 172, 91, 248, 214, 98, 41, 16, 132, 86, 203] }

payoutSc :: PlutusV2.PubKeyHash
payoutSc = PlutusV2.PubKeyHash { PlutusV2.getPubKeyHash = createBuiltinByteString [32, 243, 21, 227, 76, 76, 166, 181, 93, 144, 215, 18, 88, 137, 81, 127, 38, 249, 7, 151, 92, 121, 60, 100, 140, 197, 135, 68] }

payoutAddr :: PlutusV2.Address
payoutAddr = createAddress payoutPkh payoutSc
-------------------------------------------------------------------------------
-- | Find payout of some exact value.
-------------------------------------------------------------------------------
{-# INLINABLE findPayout #-}
findPayout :: [PlutusV2.TxOut] -> PlutusV2.Address -> PlutusV2.Value -> Bool
findPayout list addr val = helper list
  where
    helper :: [PlutusV2.TxOut] -> Bool
    helper [] = False
    helper (x:xs)
      | checkAddr && checkVal = True
      | otherwise             = helper xs
      where
        checkAddr :: Bool
        checkAddr = PlutusV2.txOutAddress x == addr

        checkVal :: Bool
        checkVal = PlutusV2.txOutValue x == val
-------------------------------------------------------------------------------
-- | Create the stake data.
-------------------------------------------------------------------------------
data StakeData = StakeData
  { stakeCred :: PlutusV2.ValidatorHash
  -- ^ The staking credential of the script.
  }
PlutusTx.unstableMakeIsData ''StakeData
-------------------------------------------------------------------------------
-- | Create the redeemer type.
-------------------------------------------------------------------------------
data CustomRedeemerType = Withdraw StakeData |
                          Delegate StakeData
PlutusTx.makeIsDataIndexed ''CustomRedeemerType [ ( 'Withdraw, 0 )
                                                , ( 'Delegate, 1 )
                                                ]
-------------------------------------------------------------------------------
-- | mkPolicy :: Redeemer -> Context -> Bool
-------------------------------------------------------------------------------
{-# INLINABLE mkPolicy #-}
mkPolicy :: BuiltinData -> PlutusV2.ScriptContext -> Bool
mkPolicy redeemer' context =
  case redeemer of
    -- | This handles the withdrawl of staking rewards.
    (Withdraw sd) -> let !stakingCred = PlutusV2.StakingHash $ PlutusV2.ScriptCredential $ stakeCred sd
                  in traceIfFalse "Withdrawal" (checkTheWithdrawal rewardWithdrawal stakingCred)
    
    -- | This handles the pool delegation.
    (Delegate sd) -> let !stakingCred = PlutusV2.StakingHash $ PlutusV2.ScriptCredential $ stakeCred sd
                  in traceIfFalse "Delegate" (checkTheCerts dCerts stakingCred)
  where
    redeemer :: CustomRedeemerType
    redeemer = PlutusTx.unsafeFromBuiltinData @CustomRedeemerType redeemer'
    
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo context

    txOutputs :: [PlutusV2.TxOut]
    txOutputs = PlutusV2.txInfoOutputs info

    dCerts :: [PlutusV2.DCert]
    dCerts = PlutusV2.txInfoDCert info

    rewardWithdrawal :: [(PlutusV2.StakingCredential, Integer)]
    rewardWithdrawal = AM.toList $ PlutusV2.txInfoWdrl info

    -- | Check for withdraws then check if the payout address gets the reward payout.
    checkTheWithdrawal :: [(PlutusV2.StakingCredential, Integer)] -> PlutusV2.StakingCredential -> Bool
    checkTheWithdrawal []     _  = False
    checkTheWithdrawal (x:xs) sc =
      if     traceIfFalse "Stake Key"      $ stakeCred == sc                             -- must be from this stake
        then traceIfFalse "Reward Payment" $ findPayout txOutputs payoutAddr payoutValue -- send reward to payout address
        else checkTheWithdrawal xs sc
      where
        stakeCred :: PlutusV2.StakingCredential
        stakeCred = fst x

        rewardAmt :: Integer
        rewardAmt = snd x

        payoutValue :: PlutusV2.Value
        payoutValue = adaValue rewardAmt

    -- | Loop all the certs and check if the stake is going to the right staking pool.
    checkTheCerts :: [PlutusV2.DCert] -> PlutusV2.StakingCredential -> Bool
    checkTheCerts []     _  = False
    checkTheCerts (x:xs) sc =
      if checkCert x
        then True                -- correct credential and pool
        else checkTheCerts xs sc -- loop all the certs
      where
        checkCert :: PlutusV2.DCert -> Bool
        checkCert cert = 
          case cert of
            -- check for a delegation to stake pool
            (PlutusV2.DCertDelegDelegate sc' poolId') -> 
              ( traceIfFalse "Stake Key" $ sc     == sc'     ) && -- only this cred can be staked
              ( traceIfFalse "Pool Id"   $ poolId == poolId' )    -- must delegate to specific pool id
    
            -- any other cert fails but stake registration
            _ -> False
-------------------------------------------------------------------------------
-- | Compile Information
-------------------------------------------------------------------------------
policy :: PlutusV2.StakeValidator
policy = PlutusV2.mkStakeValidatorScript $$(PlutusTx.compile [|| wrap ||])
 where
  wrap = Utils.mkUntypedStakeValidator mkPolicy

plutusScript :: Scripts.Script
plutusScript = PlutusV2.unStakeValidatorScript policy

validator :: PlutusV2.Validator
validator = PlutusV2.Validator plutusScript

scriptAsCbor :: LBS.ByteString
scriptAsCbor = serialise validator

stakingPlutusScript :: PlutusScript PlutusScriptV2
stakingPlutusScript = PlutusScriptSerialised . SBS.toShort $ LBS.toStrict scriptAsCbor

stakingScriptShortBs :: SBS.ShortByteString
stakingScriptShortBs = SBS.toShort . LBS.toStrict $ scriptAsCbor