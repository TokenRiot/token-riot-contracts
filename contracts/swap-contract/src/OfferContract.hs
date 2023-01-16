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
module OfferContract
  ( offerContractScript
  , offerContractScriptShortBs
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import           Codec.Serialise
import           Cardano.Api.Shelley            ( PlutusScript (..), PlutusScriptV2 )
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.ByteString.Short          as SBS
-- import qualified Plutus.V1.Ledger.Scripts       as Scripts
import           Plutus.V1.Ledger.Value         as Value
import qualified Plutus.V2.Ledger.Api           as PlutusV2
-- import qualified Plutus.V2.Ledger.Contexts      as ContextsV2
import           Plutus.Script.Utils.V2.Scripts as Utils
import           SwappableDataType
import           UsefulFuncs
import           ReducedData
import qualified Plutonomy
{- |
  Author   : The Ancient Kraken
  Copyright: 2023
-}
starterPid :: PlutusV2.CurrencySymbol
starterPid = PlutusV2.CurrencySymbol {PlutusV2.unCurrencySymbol = createBuiltinByteString [129, 132, 79, 125, 206, 228, 193, 214, 149, 152, 114, 234, 137, 214, 62, 123, 191, 183, 133, 227, 46, 115, 31, 218, 253, 43, 75, 73] }

starterTkn :: PlutusV2.TokenName
starterTkn = PlutusV2.TokenName {PlutusV2.unTokenName = createBuiltinByteString [1, 63, 255, 150, 165, 160, 105, 48, 105, 254, 161, 105, 53, 193, 115, 247, 118, 115, 157, 94, 76, 238, 92, 133, 168, 7, 97, 2, 227, 30, 23, 47] }

-- starter nft
starterValue :: PlutusV2.Value
starterValue = Value.singleton starterPid starterTkn (1 :: Integer)

-- the script reference contract
refValidatorHash :: PlutusV2.ValidatorHash
refValidatorHash = PlutusV2.ValidatorHash $ createBuiltinByteString [254, 238, 169, 123, 27, 228, 117, 143, 166, 88, 214, 238, 249, 83, 26, 159, 80, 54, 171, 210, 216, 91, 49, 189, 229, 199, 47, 237]

-- new data

-- tx info
data OfferTxInfo = OfferTxInfo
    { txInfoInputs          :: [SwapTxInInfo] -- Transaction inputs
    , txInfoReferenceInputs :: [SwapTxInInfo] -- ^ Transaction reference inputs
    , txInfoOutputs         :: [SwapTxOut] -- Transaction outputs
    , txInfoFee             :: BuiltinData
    , txInfoMint            :: BuiltinData
    , txInfoDCert           :: BuiltinData
    , txInfoWdrl            :: BuiltinData
    , txInfoValidRange      :: BuiltinData
    , txInfoSignatories     :: [PlutusV2.PubKeyHash] -- Signatures provided with the transaction, attested that they all signed the tx
    , txInfoRedeemers       :: BuiltinData
    , txInfoData            :: BuiltinData
    , txInfoId              :: BuiltinData
    }
PlutusTx.unstableMakeIsData ''OfferTxInfo

-- script context
data OfferScriptContext = OfferScriptContext
  { scriptContextTxInfo  :: OfferTxInfo
  , scriptContextPurpose :: SwapScriptPurpose
  }
PlutusTx.unstableMakeIsData ''OfferScriptContext

-- rewrite findOwnInput without higher order functions
{-# inlinable findValidatingInput #-}
findValidatingInput :: OfferScriptContext -> SwapTxOut
findValidatingInput (OfferScriptContext t_info (Spending o_ref)) = thisScriptInput (txInfoInputs t_info) o_ref
-------------------------------------------------------------------------------
-- | Create the redeemer parameters data object.
-------------------------------------------------------------------------------
data CustomRedeemerType = Remove            |
                          Complete          |
                          Transform         |
                          Update ADAIncData   
PlutusTx.makeIsDataIndexed ''CustomRedeemerType [ ( 'Remove,    0 )
                                                , ( 'Complete,  1 )
                                                , ( 'Transform, 2 )
                                                , ( 'Update,    3 )
                                                ]
-------------------------------------------------------------------------------
-- | mkValidator :: Datum -> Redeemer -> ScriptContext -> Bool
-------------------------------------------------------------------------------
{-# INLINABLE mkValidator #-}
mkValidator :: OfferDatumType -> CustomRedeemerType -> OfferScriptContext -> Bool
mkValidator datum redeemer context =
  case datum of
    {- | Offering State

      Allows many users to store their offers inside the contract for some offer 
      trade to occur. An offerer may remove their current offers or transform their
      offer into a new offer. To ensure that a specific offer goes to a specific 
      UTxO, the TxID needs to attached of the UTxO an offerer is making an offer.

      The TxId in the Offering datum is the TxId of the UTxO that an offer is 
      being made on. If the TxId changes then the offerer will need to transform
      their offer to account for TxId change.

    -}
    (Offering ptd mod _) -> let !walletPkh  = ptPkh ptd
                                !walletAddr = createAddress walletPkh (ptSc ptd)
                                !txSigners  = txInfoSignatories info
      in case redeemer of
        -- | Remove the UTxO from the contract.
        Remove -> (signedBy txSigners walletPkh)                                   -- wallet must sign it
               && (isAddrGettingPaidExactly' txOutputs walletAddr validatingValue) -- wallet must get the UTxO
               && (isNInputs' txInputs 1)                                          -- single input 
               && (isNOutputs' contTxOutputs 0)                                    -- no cont output
      
        -- | Transform the make offer tx ref info
        Transform -> 
          case getOutboundDatum contTxOutputs of
            -- offering only
            (Offering ptd' _ _) -> (signedBy txSigners walletPkh) -- wallet must sign it
                                && (ptd == ptd')                  -- wallet + stake can't change
                                && (isNInputs' txInputs 1)        -- single tx going in
                                && (isNOutputs' contTxOutputs 1)  -- single going out
            
        
        -- | Complete an offer with a specific swappable UTxO.
        Complete -> let !txId = createTxOutRef (moTx mod) (moIdx mod)
          in case getDatumByTxId txId of
            -- swappable only
            (Swappable ptd' _ _) -> let !sellerPkh  = ptPkh ptd'
                                        !sellerAddr = createAddress sellerPkh (ptSc ptd') in
                                    traceIfFalse "Incorrect Tx Signer" (signedBy txSigners sellerPkh)                                  -- The seller must sign it 
                                 && traceIfFalse "Offer Not Returned"  (isAddrGettingPaidExactly' txOutputs sellerAddr validatingValue) -- token must go back to printer
                                 && traceIfFalse "Single Script UTxO"  (isNInputs' txInputs 2)                                          -- single script input
        
        -- | A trader may update their UTxO, holding validating value constant, incrementing the min ada, and changing the payment datum.
        (Update aid) -> let !incomingValue          = validatingValue + adaValue (adaInc aid)
                            !swapHash               = swapValidatorHash refValidatorDatum
                            !(swapDatum, swapValue) = head $ thoseScriptOutputs swapHash txOutputs
          in case swapDatum of
            NoOutputDatum                    -> False
            -- (PlutusV2.OutputDatumHash _) -> False
            (OutputDatum (PlutusV2.Datum d)) ->
              case PlutusTx.fromBuiltinData d of
                Nothing     -> False
                Just inline -> 
                  case PlutusTx.unsafeFromBuiltinData @SwapDatumType inline of
                    -- update the payment data on a swappable state
                    (Swappable ptd' _ td') -> traceIfFalse "sign"  (signedBy txSigners walletPkh)                       -- seller must sign it
                                           && traceIfFalse "owner" (ptd == ptd') -- seller can not change
                                           && traceIfFalse "time"  (checkValidTimeData td')                       -- valid time data
                                           && traceIfFalse "ins"   (isNInputs' txInputs 1) -- single tx going in, single going out
                                           && traceIfFalse "value" (swapValue == incomingValue) -- values must match
    
  where
    info :: OfferTxInfo
    info = scriptContextTxInfo context

    txOutputs :: [SwapTxOut]
    txOutputs = txInfoOutputs info

    txInputs :: [SwapTxInInfo]
    txInputs = txInfoInputs info

    txReferences :: [SwapTxInInfo]
    txReferences = txInfoReferenceInputs info

    refValidatorDatum :: ScriptRefDatumType
    refValidatorDatum = 
      if (length txReferences == 1) && checkReferenceValue
        then 
          case txOutDatum $ txInInfoResolved $ head txReferences of
            NoOutputDatum       -> traceError "No Datum"
            -- (PlutusV2.OutputDatumHash _) -> traceError "Embedded Datum"
            -- inline datum only
            (OutputDatum (PlutusV2.Datum d)) -> 
              case PlutusTx.fromBuiltinData d of
                Nothing     -> traceError "Bad Data"
                Just inline -> PlutusTx.unsafeFromBuiltinData @ScriptRefDatumType inline
        else traceError "Bad Reference Input"

    checkReferenceValue :: Bool
    checkReferenceValue = Value.geq refValue starterValue
      where refValue = txOutValue $ txInInfoResolved $ head txReferences

    validatingInput :: SwapTxOut
    validatingInput = findValidatingInput context

    validatingValue :: PlutusV2.Value
    validatingValue = txOutValue validatingInput
    
    validatingAddress :: PlutusV2.Address
    validatingAddress = txOutAddress validatingInput

    contTxOutputs :: [SwapTxOut]
    contTxOutputs = theseScriptOutputs txOutputs validatingAddress

    -- Create a TxOutRef from the tx hash and index.
    createTxOutRef :: PlutusV2.BuiltinByteString -> Integer -> PlutusV2.TxOutRef
    createTxOutRef txHash index = txId
      where
        txId :: PlutusV2.TxOutRef
        txId = PlutusV2.TxOutRef
          { PlutusV2.txOutRefId  = PlutusV2.TxId { PlutusV2.getTxId = txHash }
          , PlutusV2.txOutRefIdx = index
          }

    getOutboundDatum :: [SwapTxOut] -> OfferDatumType
    getOutboundDatum []     = traceError "Nothing Found"
    getOutboundDatum (x:xs) =
      case txOutDatum x of
        NoOutputDatum       -> getOutboundDatum xs
        -- (PlutusV2.OutputDatumHash _) -> traceError "Embedded Datum"
        -- inline datum only
        (OutputDatum (PlutusV2.Datum d)) -> 
          case PlutusTx.fromBuiltinData d of
            Nothing     -> traceError "Bad Data"
            Just inline -> PlutusTx.unsafeFromBuiltinData @OfferDatumType inline

    getDatumByTxId :: PlutusV2.TxOutRef -> SwapDatumType
    getDatumByTxId txId = 
      case txOutDatum $ txInInfoResolved $ swapTxInFromTxRef txInputs txId of
        NoOutputDatum       -> traceError "No Datum"
        -- (PlutusV2.OutputDatumHash _) -> traceError "Embedded Datum"
        -- inline datum only
        (OutputDatum (PlutusV2.Datum d)) -> 
          case PlutusTx.fromBuiltinData d of
            Nothing     -> traceError "Bad Data"
            Just inline -> PlutusTx.unsafeFromBuiltinData @SwapDatumType inline
-------------------------------------------------------------------------------
-- | Now we need to compile the Validator.
-------------------------------------------------------------------------------
-- offerValidator :: PlutusV2.Validator
-- offerValidator = PlutusV2.mkValidatorScript
--     $$(PlutusTx.compile [|| wrap ||])
--  where
--     wrap = Utils.mkUntypedValidator mkValidator
-- -------------------------------------------------------------------------------
-- -- | The code below is required for the plutus script compile.
-- -------------------------------------------------------------------------------
-- script :: Scripts.Script
-- script = Scripts.unValidatorScript offerValidator

-- offerContractScriptShortBs :: SBS.ShortByteString
-- offerContractScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

-- offerContractScript :: PlutusScript PlutusScriptV2
-- offerContractScript = PlutusScriptSerialised offerContractScriptShortBs

-- -------------------------------------------------------------------------------
-- -- | Now we need to compile the Validator.
-- -------------------------------------------------------------------------------
wrappedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedValidator x y z = check (mkValidator (PlutusV2.unsafeFromBuiltinData x) (PlutusV2.unsafeFromBuiltinData y) (PlutusV2.unsafeFromBuiltinData z))

validator :: Validator
-- validator = Plutonomy.optimizeUPLC $ Plutonomy.validatorToPlutus $ Plutonomy.mkValidatorScript $$(PlutusTx.compile [|| wrappedValidator ||])
validator = Plutonomy.optimizeUPLCWith Plutonomy.aggressiveOptimizerOptions $ Plutonomy.validatorToPlutus $ Plutonomy.mkValidatorScript $$(PlutusTx.compile [|| wrappedValidator ||])

offerContractScriptShortBs :: SBS.ShortByteString
offerContractScriptShortBs = SBS.toShort . LBS.toStrict $ serialise validator

offerContractScript :: PlutusScript PlutusScriptV2
offerContractScript = PlutusScriptSerialised offerContractScriptShortBs