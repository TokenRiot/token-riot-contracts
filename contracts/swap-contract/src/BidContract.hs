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
module BidContract
  ( bidContractScript
  , bidContractScriptShortBs
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import           Codec.Serialise
import           Cardano.Api.Shelley            ( PlutusScript (..), PlutusScriptV2 )
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.ByteString.Short          as SBS
import qualified Plutus.V1.Ledger.Scripts       as Scripts
import           Plutus.V1.Ledger.Value         as Value
import qualified Plutus.V2.Ledger.Api           as PlutusV2
import qualified Plutus.V2.Ledger.Contexts      as ContextsV2
import           Plutus.Script.Utils.V2.Scripts as Utils
import           SwappableDataType
import           UsefulFuncs
import           ReducedData
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
-- The new data types



-------------------------------------------------------------------------------
-- | Create the redeemer parameters data object.
-------------------------------------------------------------------------------
data CustomRedeemerType = Remove    |
                          Complete  |
                          Transform |
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
mkValidator :: BidDatumType -> CustomRedeemerType -> PlutusV2.ScriptContext -> Bool
mkValidator datum redeemer context =
  case datum of
    {- | Bidding State 

      Allows a bidder to place their bid into the contract for an on going auction. At the end of
      the auction, the auctioner will select the best bid and complete the auction or they may reject
      all bids and remove the UTxO back into the swap state. Similar to the offer state, bidders will
      need to remove old bids when the auction is over. They may also choose to transform their bid for
      a new auction or to increase the bid by changing the value.
    
    -}
    (Bidding ptd mod) -> let !walletPkh  = ptPkh ptd
                             !walletAddr = createAddress walletPkh (ptSc ptd)
                             !txSigners  = ContextsV2.txInfoSignatories info
      in case redeemer of
        -- | Remove the UTxO from the contract.
        Remove -> (signedBy txSigners walletPkh)                                  -- wallet must sign it
               && (isAddrGettingPaidExactly txOutputs walletAddr validatingValue) -- wallet must get the UTxO
               && (isNInputs txInputs 1 && isNOutputs contTxOutputs 0)            -- single input no cont output
      
        -- | Transform the auction bid
        Transform -> 
          case getOutboundDatum contTxOutputs of
            -- offering only
            (Bidding ptd' _) -> (signedBy txSigners walletPkh)                       -- wallet must sign it
                             && (ptd == ptd')                                        -- wallet + stake can't change
                             && (isNInputs txInputs 1 && isNOutputs contTxOutputs 1) -- single tx going in, single going out
            
        -- | Update a bid into the swap contract
        (Update aid) -> let !incomingValue          = validatingValue + adaValue (adaInc aid)
                            !swapHash               = swapValidatorHash refValidatorDatum
                            !(swapDatum, swapValue) = head $ ContextsV2.scriptOutputsAt swapHash info
          in case swapDatum of
            PlutusV2.NoOutputDatum       -> False
            (PlutusV2.OutputDatumHash _) -> False
            (PlutusV2.OutputDatum (PlutusV2.Datum d)) ->
              case PlutusTx.fromBuiltinData d of
                Nothing     -> False
                Just inline -> 
                  case PlutusTx.unsafeFromBuiltinData @SwapDatumType inline of
                    -- update the payment data on a swappable state
                    (Swappable ptd' _ td') -> (signedBy txSigners walletPkh) -- seller must sign it
                                           && (ptd == ptd')                  -- seller can not change
                                           && (checkValidTimeData td')       -- valid time data
                                           && (isNInputs txInputs 1)         -- single tx going in
                                           && swapValue == incomingValue -- values must match    

        -- | Complete an offer with a specific swappable UTxO.
        Complete -> let !txId = createTxOutRef (moTx mod) (moIdx mod)
          in case getDatumByTxId txId of
            -- auction only
            (Auctioning ptd' atd _) -> let !sellerPkh  = ptPkh ptd'
                                           !sellerAddr = createAddress sellerPkh (ptSc ptd') 
                                           !auctionTimeInterval = lockBetweenTimeInterval (tStart atd) (tEnd atd)
                                           !txValidityRange     = ContextsV2.txInfoValidRange info
                                    in traceIfFalse "Incorrect Tx Signer" (signedBy txSigners sellerPkh)                                  -- The seller must sign it 
                                    && traceIfFalse "Offer Not Returned"  (isAddrGettingPaidExactly txOutputs sellerAddr validatingValue) -- token must go back to printer
                                    && traceIfFalse "Single Script UTxO"  (isNInputs txInputs 2)                                          -- single script input
                                    && traceIfFalse "Auction Is Live"     (isTxOutsideInterval auctionTimeInterval txValidityRange)       -- a wallet can unlock it
  where
    info :: PlutusV2.TxInfo
    info = ContextsV2.scriptContextTxInfo  context

    txOutputs :: [PlutusV2.TxOut]
    txOutputs = ContextsV2.txInfoOutputs info

    txInputs :: [PlutusV2.TxInInfo]
    txInputs = ContextsV2.txInfoInputs info

    txReferences :: [PlutusV2.TxInInfo]
    txReferences = ContextsV2.txInfoReferenceInputs info

    refValidatorDatum :: ScriptRefDatumType
    refValidatorDatum = 
      if (length txReferences == 1) && checkReferenceValue
        then 
          case ContextsV2.txOutDatum $ ContextsV2.txInInfoResolved $ head txReferences of
            PlutusV2.NoOutputDatum       -> traceError "No Datum"
            (PlutusV2.OutputDatumHash _) -> traceError "Embedded Datum"
            -- inline datum only
            (PlutusV2.OutputDatum (PlutusV2.Datum d)) -> 
              case PlutusTx.fromBuiltinData d of
                Nothing     -> traceError "Bad Data"
                Just inline -> PlutusTx.unsafeFromBuiltinData @ScriptRefDatumType inline
        else traceError "Bad Reference Input"

    checkReferenceValue :: Bool
    checkReferenceValue = Value.geq refValue starterValue
      where refValue = ContextsV2.txOutValue $ ContextsV2.txInInfoResolved $ head txReferences

    validatingInput :: PlutusV2.TxOut
    validatingInput = ownInput context

    validatingValue :: PlutusV2.Value
    validatingValue = PlutusV2.txOutValue validatingInput
    
    validatingAddress :: PlutusV2.Address
    validatingAddress = PlutusV2.txOutAddress validatingInput

    contTxOutputs :: [PlutusV2.TxOut]
    contTxOutputs = getScriptOutputs txOutputs validatingAddress

    -- Create a TxOutRef from the tx hash and index.
    createTxOutRef :: PlutusV2.BuiltinByteString -> Integer -> PlutusV2.TxOutRef
    createTxOutRef txHash index = txId
      where
        txId :: PlutusV2.TxOutRef
        txId = PlutusV2.TxOutRef
          { PlutusV2.txOutRefId  = PlutusV2.TxId { PlutusV2.getTxId = txHash }
          , PlutusV2.txOutRefIdx = index
          }

    getOutboundDatum :: [PlutusV2.TxOut] -> BidDatumType
    getOutboundDatum []     = traceError "Nothing Found"
    getOutboundDatum (x:xs) =
      case PlutusV2.txOutDatum x of
        PlutusV2.NoOutputDatum       -> getOutboundDatum xs
        (PlutusV2.OutputDatumHash _) -> traceError "Embedded Datum"
        -- inline datum only
        (PlutusV2.OutputDatum (PlutusV2.Datum d)) -> 
          case PlutusTx.fromBuiltinData d of
            Nothing     -> traceError "Bad Data"
            Just inline -> PlutusTx.unsafeFromBuiltinData @BidDatumType inline

    getDatumByTxId :: PlutusV2.TxOutRef -> AuctionDatumType
    getDatumByTxId txId = 
      case PlutusV2.txOutDatum $ PlutusV2.txInInfoResolved $ txInFromTxRef txInputs txId of
        PlutusV2.NoOutputDatum       -> traceError "No Datum"
        (PlutusV2.OutputDatumHash _) -> traceError "Embedded Datum"
        -- inline datum only
        (PlutusV2.OutputDatum (PlutusV2.Datum d)) -> 
          case PlutusTx.fromBuiltinData d of
            Nothing     -> traceError "Bad Data"
            Just inline -> PlutusTx.unsafeFromBuiltinData @AuctionDatumType inline
-------------------------------------------------------------------------------
-- | Now we need to compile the Validator.
-------------------------------------------------------------------------------
bidValidator :: PlutusV2.Validator
bidValidator = PlutusV2.mkValidatorScript
    $$(PlutusTx.compile [|| wrap ||])
 where
    wrap = Utils.mkUntypedValidator mkValidator
-------------------------------------------------------------------------------
-- | The code below is required for the plutus script compile.
-------------------------------------------------------------------------------
script :: Scripts.Script
script = Scripts.unValidatorScript bidValidator

bidContractScriptShortBs :: SBS.ShortByteString
bidContractScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

bidContractScript :: PlutusScript PlutusScriptV2
bidContractScript = PlutusScriptSerialised bidContractScriptShortBs
