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
{-# LANGUAGE RecordWildCards       #-}
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
module StakeContract
  ( stakingPlutusScript
  , ScriptParameters(..)
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import           Codec.Serialise
import           Cardano.Api.Shelley     ( PlutusScript (..), PlutusScriptV2 )
import qualified Data.ByteString.Lazy    as LBS
import qualified Data.ByteString.Short   as SBS
import qualified PlutusTx.AssocMap       as AM
import qualified Plutus.V1.Ledger.Value  as Value
import qualified Plutus.V2.Ledger.Api    as V2
import           UsefulFuncs             ( adaValue, createAddress )
import           OptimizerOptions        ( theOptimizerOptions )
import           Plutonomy
import           ReferenceDataType
import           ReducedFunctions
-------------------------------------------------------------------------------
-- | Starter NFT Contract Parameterization
-------------------------------------------------------------------------------
data ScriptParameters = ScriptParameters
  { lockPid   :: V2.CurrencySymbol
  -- ^ The locking token's policy id.
  , lockTkn   :: V2.TokenName
  -- ^ The locking token's token name
  , refHash   :: V2.ValidatorHash
  -- ^ The validator hash of the data reference contract
  }
PlutusTx.makeLift ''ScriptParameters
-------------------------------------------------------------------------------
-- | Create the stake data.
-------------------------------------------------------------------------------
data StakeData = StakeData
  { stakeCred :: V2.ValidatorHash
  -- ^ The staking credential of the script.
  }
PlutusTx.makeIsDataIndexed ''StakeData [('StakeData, 0)]

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
mkPolicy :: ScriptParameters -> BuiltinData -> V2.ScriptContext -> Bool
mkPolicy ScriptParameters {..} redeemer' context =
  case redeemer of
    -- | This handles the withdrawl of staking rewards.
    (Withdraw sd) ->
      let !stakingCred          = V2.StakingHash $ V2.ScriptCredential $ stakeCred sd
          !info                 = V2.scriptContextTxInfo context
          !rewardWithdrawal     = AM.toList $ V2.txInfoWdrl info
          !txOutputs            = V2.txInfoOutputs info
          !refTxIns             = V2.txInfoReferenceInputs info
          !refTxOut             = getReferenceInput refTxIns refHash
          !(Reference _ _ _ sp) = getReferenceDatum refTxOut
          !refValue             = V2.txOutValue refTxOut
          !payoutAddr           = createAddress (rewardPkh sp) (rewardSc sp)
      in traceIfFalse "wit" (checkTheWithdrawal rewardWithdrawal stakingCred txOutputs payoutAddr)  -- check if correct withdrawal
      && traceIfFalse "val" (Value.valueOf refValue lockPid lockTkn == 1)                           -- check if correct reference
    
    -- | This handles the pool delegation.
    (Delegate sd) ->
      let !stakingCred          = V2.StakingHash $ V2.ScriptCredential $ stakeCred sd
          !info                 = V2.scriptContextTxInfo context
          !dCerts               = V2.txInfoDCert info
          !refTxIns             = V2.txInfoReferenceInputs info
          !refTxOut             = getReferenceInput refTxIns refHash
          !(Reference _ _ _ sp) = getReferenceDatum refTxOut
          !refValue             = V2.txOutValue refTxOut
          !pool                 = (poolId sp)
      in traceIfFalse "del" (checkTheCerts dCerts stakingCred pool)        -- check if correct delegation
      && traceIfFalse "val" (Value.valueOf refValue lockPid lockTkn == 1)  -- check if correct reference
  where
    redeemer :: CustomRedeemerType
    redeemer = PlutusTx.unsafeFromBuiltinData @CustomRedeemerType redeemer'

    getReferenceDatum :: V2.TxOut -> ReferenceDatum
    getReferenceDatum x = 
      case V2.txOutDatum x of
        V2.NoOutputDatum       -> traceError "No Datum"
        (V2.OutputDatumHash _) -> traceError "Embedded Datum"
        (V2.OutputDatum (V2.Datum d)) -> 
          case PlutusTx.fromBuiltinData d of
            Nothing     -> traceError "Bad Data"
            Just inline -> PlutusTx.unsafeFromBuiltinData @ReferenceDatum inline
    
    -- | Check for withdraws then check if the payout address gets the reward payout.
    checkTheWithdrawal :: [(V2.StakingCredential, Integer)] -> V2.StakingCredential -> [V2.TxOut] -> V2.Address -> Bool
    checkTheWithdrawal []     _  _      _    = False
    checkTheWithdrawal (x:xs) sc txOuts addr =
      if stakeCred == sc                        -- must be from this stake
        then findPayout txOuts addr payoutValue -- send reward to payout address
        else checkTheWithdrawal xs sc txOuts addr
      where
        stakeCred :: V2.StakingCredential
        stakeCred = fst x

        rewardAmt :: Integer
        rewardAmt = snd x

        payoutValue :: V2.Value
        payoutValue = adaValue rewardAmt

    -- | Loop all the certs and check if the stake is going to the right staking pool.
    checkTheCerts :: [V2.DCert] -> V2.StakingCredential -> V2.PubKeyHash -> Bool
    checkTheCerts []     _  _    = False
    checkTheCerts (x:xs) sc pool =
      if checkCert x
        then True                     -- correct credential and pool
        else checkTheCerts xs sc pool -- loop all the certs
      where
        checkCert :: V2.DCert -> Bool
        checkCert cert = 
          case cert of
            -- check for a delegation to stake pool
            (V2.DCertDelegDelegate sc' pool') -> (sc   == sc'  ) -- only this cred can be staked
                                              && (pool == pool') -- must delegate to specific pool id
    
            -- any other cert fails but stake registration
            _ -> False
-------------------------------------------------------------------------------
-- | Compile Information
-------------------------------------------------------------------------------
wrappedPolicy :: ScriptParameters -> BuiltinData -> BuiltinData -> ()
wrappedPolicy s x y = check (mkPolicy s (V2.unsafeFromBuiltinData x) (V2.unsafeFromBuiltinData y))

policy :: ScriptParameters -> V2.StakeValidator
policy sp = V2.mkStakeValidatorScript $
  $$(PlutusTx.compile [|| wrappedPolicy ||])
  `PlutusTx.applyCode`
  PlutusTx.liftCode sp

stakingPlutusScript :: ScriptParameters -> PlutusScript PlutusScriptV2
stakingPlutusScript sp = PlutusScriptSerialised . SBS.toShort $ LBS.toStrict $ serialise $ 
  Plutonomy.optimizeUPLCWith theOptimizerOptions $ V2.Validator $ V2.unStakeValidatorScript (policy sp)
