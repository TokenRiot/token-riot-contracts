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
module SwapContract
  ( swapContractScript
  , swapContractScriptShortBs
  , swapValidator
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
refValidatorHash = PlutusV2.ValidatorHash $ createBuiltinByteString [15, 196, 149, 183, 139, 121, 96, 63, 128, 105, 77, 227, 78, 254, 112, 252, 143, 229, 251, 55, 247, 132, 32, 237, 180, 140, 121, 15]
-------------------------------------------------------------------------------
-- | Create the redeemer parameters data object.
-------------------------------------------------------------------------------
data CustomRedeemerType = Remove                                         |
                          TransformUTxO                                  |
                          UpdatePrice ADAIncData                         |
                          ChangeState ADAIncData                         |
                          FlatRate    PayToData ADAIncData SpecificToken |
                          FRRemove    PayToData SpecificToken            |
                          Offer       ADAIncData MakeOfferData           |
                          ORemove     MakeOfferData
PlutusTx.makeIsDataIndexed ''CustomRedeemerType [ ( 'Remove,        0 )
                                                , ( 'TransformUTxO, 1 )
                                                , ( 'UpdatePrice,   2 )
                                                , ( 'ChangeState,   3 )
                                                , ( 'FlatRate,      4 )
                                                , ( 'FRRemove,      5 )
                                                , ( 'Offer,         6 )
                                                , ( 'ORemove,       7 )
                                                ]
-------------------------------------------------------------------------------
-- | mkValidator :: Datum -> Redeemer -> ScriptContext -> Bool
-------------------------------------------------------------------------------
{-# INLINABLE mkValidator #-}
mkValidator :: SwapDatumType -> CustomRedeemerType -> PlutusV2.ScriptContext -> Bool
mkValidator datum redeemer context =
  case datum of
    {- | Swapping State

      Different ways to swap UTxO walletship.

      Redeemers will determine the type of swap being used. Unless the UTxO is being
      explicity removed, the UTxO is assumed to be continuing inside the contract.

      User's are allowed to update and remove their utxos at will if not time locked.

      To Lock some UTxO for some pre-defined interval of time. The datum expects
      input integers that are greater than or equal to zero.

      echo `expr $(echo $(date +%s%3N)) + $(echo 0)`
      # 1659817471786

      A five (5) minute window would be 5 * 60 * 1000  = 300,000.
      
      echo `expr $(echo $(date +%s%3N)) + $(echo 300000)`
      # 1659817771786
    -}
    (Swappable ptd pd td) -> let !walletPkh        = ptPkh ptd
                                 !walletAddr       = createAddress walletPkh (ptSc ptd)
                                 !lockTimeInterval = lockBetweenTimeInterval (tStart td) (tEnd td)
                                 !txValidityRange  = ContextsV2.txInfoValidRange info
                                 !txSigners        = ContextsV2.txInfoSignatories info
      in case redeemer of
        -- | A trader may remove their UTxO if not currently being timelocked.
        Remove -> (signedBy txSigners walletPkh)                                  -- seller must sign it
               && (isAddrGettingPaidExactly txOutputs walletAddr validatingValue) -- seller must get the UTxO
               && (isNInputs txInputs 1)                                          -- single tx going in
               && (isNOutputs contTxOutputs 0)                                    -- no continue
               && (isTxOutsideInterval lockTimeInterval txValidityRange)          -- seller can unlock it
        
        -- | A trader may transform their UTxO, holding the owner constant, changing the value and time.
        TransformUTxO -> 
          case getOutboundDatum contTxOutputs of
            -- transform a swappable utxo
            (Swappable ptd' _ td') -> (signedBy txSigners walletPkh)                         -- seller must sign it
                                   && (isNInputs txInputs 1 && isNOutputs contTxOutputs 1)   -- single tx going in, single going out
                                   && (ptd == ptd')                                          -- seller cant change
                                   && (checkValidTimeLock td td')                            -- valid time lock
                                   && (isTxOutsideInterval lockTimeInterval txValidityRange) -- seller can unlock it
                
        -- | A trader may update their UTxO, holding validating value constant, incrementing the min ada, and changing the payment datum.
        (UpdatePrice aid) -> let !incomingValue = validatingValue + adaValue (adaInc aid)
          in case getOutboundDatumByValue contTxOutputs incomingValue of
            -- update the payment data on a swappable state
            (Swappable ptd' _ td') -> (signedBy txSigners walletPkh)                       -- seller must sign it
                                   && ((ptd == ptd') && (td == td'))                       -- seller and time can't change
                                   && (isNInputs txInputs 1 && isNOutputs contTxOutputs 1) -- single tx going in, single going out

        -- change into the auction state
        (ChangeState aid)-> let !incomingValue                = validatingValue + adaValue (adaInc aid)
                                !auctionHash                  = auctionValidatorHash refValidatorDatum
                                !(auctionDatum, auctionValue) = head $ ContextsV2.scriptOutputsAt auctionHash info
          in case auctionDatum of
            PlutusV2.NoOutputDatum       -> False
            (PlutusV2.OutputDatumHash _) -> False
            (PlutusV2.OutputDatum (PlutusV2.Datum d)) ->
              case PlutusTx.fromBuiltinData d of
                Nothing     -> False
                Just inline -> 
                  case PlutusTx.unsafeFromBuiltinData @AuctionDatumType inline of
                    -- update the payment data on a auctionpable state
                    (Auctioning ptd' atd td') -> (signedBy txSigners walletPkh)                         -- seller must sign it
                                              && (ptd == ptd')                                          -- seller and time can't change
                                              && (isNInputs txInputs 1)   -- single tx going in, single going out
                                              && (checkValidTimeData atd)                               -- valid auction time lock
                                              && (td == td')                            -- valid global time lock
                                              && auctionValue == incomingValue -- values must match   
        
        -- | Flat rate swap of UTxO for an predefined amount of a single token.
        (FlatRate ptd' aid st) -> let !incomingValue = validatingValue + adaValue (adaInc aid)
                                      !thisTkn       = getTokenName pd st
          in case getOutboundDatumByValue contTxOutputs incomingValue of
            -- swappable only
            (Swappable ptd'' _ td') -> (isAddrHoldingExactlyToken txOutputs walletAddr (pPid pd) thisTkn (pAmt pd)) -- seller must be paid
                                    && ((ptd /= ptd'') && (ptd' == ptd'') && (td == td'))                           -- seller change but remain locked
                                    && ((pAmt pd) /= 0)                                                             -- seller must define price
                                    && (isNInputs txInputs 1 && isNOutputs contTxOutputs 1)                         -- single tx going in, single going out
                                    && (signedBy txSigners (ptPkh ptd'))                                            -- buyer must sign

        -- | Flat rate purchase into buyer wallet of UTxO for an predefined amount of a single token.
        (FRRemove ptd' st) -> let !buyerPkh  = ptPkh ptd'
                                  !buyerAddr = createAddress buyerPkh (ptSc ptd')
                                  !thisTkn   = getTokenName pd st
                           in (signedBy txSigners buyerPkh)                                                -- buyer must sign
                           && (isAddrHoldingExactlyToken txOutputs walletAddr (pPid pd) thisTkn (pAmt pd)) -- seller must be paid
                           && (isAddrGettingPaidExactly txOutputs buyerAddr validatingValue)               -- buyer must be paid
                           && ((pAmt pd) /= 0)                                                             -- seller must define price
                           && (isNInputs txInputs 1 && isNOutputs contTxOutputs 0)                         -- single tx going in, no continue
                           && (isTxOutsideInterval lockTimeInterval txValidityRange)                       -- seller can unlock UTxO

        -- | Offer to change walletship of UTxO for some amount of a single token + extras.
        (Offer aid mod) -> let !txId = createTxOutRef (moTx mod) (moIdx mod)
          in case getDatumByTxId txId of
            -- offering only
            (Offering ptd' _ ofd) -> let !incomingValue = validatingValue + adaValue (adaInc aid)
              in case getOutboundDatumByValue contTxOutputs incomingValue of
                -- cont into swappable only
                (Swappable ptd'' pd' td') -> (signedBy txSigners walletPkh)                       -- seller must sign it
                                          && ((ptd /= ptd'') && (ptd' == ptd'') && (td == td'))   -- seller change but remain locked
                                          && (isNInputs txInputs 2 && isNOutputs contTxOutputs 1) -- two tx going in, single going out
                                          && (pd' == defaultPayment)                              -- payment data must be default
                                          && (oFlag ofd == 0)                                     -- Offer stays in contract

        
        -- | Offer but remove it to a the buyer's wallet
        (ORemove mod) -> let !txId = createTxOutRef (moTx mod) (moIdx mod)
          in case getDatumByTxId txId of
            -- offering only
            (Offering ptd' _ ofd) -> let !buyerAddr = createAddress (ptPkh ptd') (ptSc ptd')
                                  in (signedBy txSigners walletPkh)                                 -- seller must sign it
                                  && (isAddrGettingPaidExactly txOutputs buyerAddr validatingValue) -- buyer must be paid
                                  && (isNInputs txInputs 2 && isNOutputs contTxOutputs 0)           -- two tx going in, no continue
                                  && (isTxOutsideInterval lockTimeInterval txValidityRange)         -- seller can unlock it
                                  && (oFlag ofd /= 0)                                               -- Offer is for the remove endpoint
  where
    info :: PlutusV2.TxInfo
    info = ContextsV2.scriptContextTxInfo  context

    txOutputs :: [PlutusV2.TxOut]
    txOutputs = ContextsV2.txInfoOutputs info

    txInputs :: [PlutusV2.TxInInfo]
    txInputs = PlutusV2.txInfoInputs info

    validatingInput :: PlutusV2.TxOut
    validatingInput = ownInput context

    validatingValue :: PlutusV2.Value
    validatingValue = PlutusV2.txOutValue validatingInput
    
    validatingAddress :: PlutusV2.Address
    validatingAddress = PlutusV2.txOutAddress validatingInput

    contTxOutputs :: [PlutusV2.TxOut]
    contTxOutputs = getScriptOutputs txOutputs validatingAddress

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

    -- Create a TxOutRef from the tx hash and index.
    createTxOutRef :: PlutusV2.BuiltinByteString -> Integer -> PlutusV2.TxOutRef
    createTxOutRef txHash index = txId
      where
        txId :: PlutusV2.TxOutRef
        txId = PlutusV2.TxOutRef
          { PlutusV2.txOutRefId  = PlutusV2.TxId { PlutusV2.getTxId = txHash }
          , PlutusV2.txOutRefIdx = index
          }

    getOutboundDatumByValue :: [PlutusV2.TxOut] -> PlutusV2.Value -> SwapDatumType
    getOutboundDatumByValue []     _   = traceError "Nothing Found"
    getOutboundDatumByValue (x:xs) val =
      if PlutusV2.txOutValue x == val -- strict value continue
        then
          case PlutusV2.txOutDatum x of
            PlutusV2.NoOutputDatum       -> traceError "No Datum"
            (PlutusV2.OutputDatumHash _) -> traceError "Embedded Datum"
            -- inline datum only
            (PlutusV2.OutputDatum (PlutusV2.Datum d)) -> 
              case PlutusTx.fromBuiltinData d of
                Nothing     -> traceError "Bad Data"
                Just inline -> PlutusTx.unsafeFromBuiltinData @SwapDatumType inline
        else getOutboundDatumByValue xs val
    
    getOutboundDatum :: [PlutusV2.TxOut] -> SwapDatumType
    getOutboundDatum []     = traceError "Nothing Found"
    getOutboundDatum (x:xs) =
      case PlutusV2.txOutDatum x of
        PlutusV2.NoOutputDatum       -> getOutboundDatum xs
        (PlutusV2.OutputDatumHash _) -> traceError "Embedded Datum"
        -- inline datum only
        (PlutusV2.OutputDatum (PlutusV2.Datum d)) -> 
          case PlutusTx.fromBuiltinData d of
            Nothing     -> traceError "Bad Data"
            Just inline -> PlutusTx.unsafeFromBuiltinData @SwapDatumType inline

    getDatumByTxId :: PlutusV2.TxOutRef -> OfferDatumType
    getDatumByTxId txId = 
      case PlutusV2.txOutDatum $ PlutusV2.txInInfoResolved $ txInFromTxRef txInputs txId of
        PlutusV2.NoOutputDatum       -> traceError "No Datum"
        (PlutusV2.OutputDatumHash _) -> traceError "Embedded Datum"
        -- inline datum only
        (PlutusV2.OutputDatum (PlutusV2.Datum d)) -> 
          case PlutusTx.fromBuiltinData d of
            Nothing     -> traceError "Bad Data"
            Just inline -> PlutusTx.unsafeFromBuiltinData @OfferDatumType inline
-------------------------------------------------------------------------------
-- | Now we need to compile the Validator.
-------------------------------------------------------------------------------
swapValidator :: PlutusV2.Validator
swapValidator = PlutusV2.mkValidatorScript
    $$(PlutusTx.compile [|| wrap ||])
 where
    wrap = Utils.mkUntypedValidator mkValidator
-------------------------------------------------------------------------------
-- | The code below is required for the plutus script compile.
-------------------------------------------------------------------------------
script :: Scripts.Script
script = Scripts.unValidatorScript swapValidator

swapContractScriptShortBs :: SBS.ShortByteString
swapContractScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

swapContractScript :: PlutusScript PlutusScriptV2
swapContractScript = PlutusScriptSerialised swapContractScriptShortBs
