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
{-# LANGUAGE NumericUnderscores    #-}
-- Options
{-# OPTIONS_GHC -fno-strictness               #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas   #-}
{-# OPTIONS_GHC -fobject-code                 #-}
{-# OPTIONS_GHC -fno-specialise               #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings       #-}
module MinterContract
  ( mintingPlutusScript
  , ScriptParameters(..)
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import           Cardano.Api.Shelley    ( PlutusScript (..), PlutusScriptV2 )
import           Codec.Serialise        ( serialise )
import qualified Data.ByteString.Lazy   as LBS
import qualified Data.ByteString.Short  as SBS
import qualified Plutus.V1.Ledger.Value as Value
import qualified Plutus.V2.Ledger.Api   as V2
import           OptimizerOptions       ( theOptimizerOptions )
import           ReducedFunctions       ( uniqueTokenName, getReferenceInput, signedBy )
import           ReferenceDataType
import qualified Plutonomy
import           UsefulFuncs            ( createBuiltinByteString )
{-
  Author   : The Ancient Kraken
  Copyright: 2023
-}
-------------------------------------------------------------------------------
-- | Starter NFT Contract Parameterization
-------------------------------------------------------------------------------
data ScriptParameters = ScriptParameters
  { lockPid :: V2.CurrencySymbol
  -- ^ The locking token's policy id.
  , lockTkn :: V2.TokenName
  -- ^ The locking token's token name
  , refHash :: V2.ValidatorHash
  -- ^ The validator hash of the data reference contract
  }
PlutusTx.makeLift ''ScriptParameters
-------------------------------------------------------------------------------
-- | Custom Data Objects
-------------------------------------------------------------------------------
data MintTxInInfo = MintTxInInfo
    { txInInfoOutRef   :: V2.TxOutRef
    , txInInfoResolved :: BuiltinData
    } 
PlutusTx.unstableMakeIsData ''MintTxInInfo

data MintTxInfo = MintTxInfo
    { txInfoInputs          :: [MintTxInInfo] -- Transaction inputs
    , txInfoReferenceInputs :: [V2.TxInInfo]  -- Transaction reference inputs
    , txInfoOutputs         :: BuiltinData
    , txInfoFee             :: BuiltinData
    , txInfoMint            :: V2.Value       -- The 'Value' minted by this transaction.
    , txInfoDCert           :: BuiltinData
    , txInfoWdrl            :: BuiltinData
    , txInfoValidRange      :: BuiltinData
    , txInfoSignatories     :: [V2.PubKeyHash]-- Transaction signers
    , txInfoRedeemers       :: BuiltinData
    , txInfoData            :: BuiltinData
    , txInfoId              :: BuiltinData
    }
PlutusTx.unstableMakeIsData ''MintTxInfo

data MintScriptPurpose = Minting V2.CurrencySymbol
PlutusTx.unstableMakeIsData ''MintScriptPurpose

data MintScriptContext = MintScriptContext
  { scriptContextTxInfo :: MintTxInfo
  , scriptContextPurpose :: MintScriptPurpose 
  }
PlutusTx.unstableMakeIsData ''MintScriptContext

ownCurrencySymbol :: MintScriptContext -> V2.CurrencySymbol
ownCurrencySymbol MintScriptContext{scriptContextPurpose=Minting cs} = cs
-------------------------------------------------------------------------------
-- | Parameters -> Data -> Context -> Bool
-------------------------------------------------------------------------------
{-# INLINABLE mkPolicy #-}
mkPolicy :: ScriptParameters -> BuiltinData -> MintScriptContext -> Bool
mkPolicy ScriptParameters {..} _ context = 
    let !info                 = scriptContextTxInfo context
        !firstTx              = txInInfoOutRef $ head $ txInfoInputs info
        !refTxIns             = txInfoReferenceInputs info
        !refTxOut             = getReferenceInput refTxIns refHash
        !refValue             = V2.txOutValue refTxOut
        !(Reference _ _ sd _) = getReferenceDatum refTxOut
        !hotPkh               = mHot sd
        !txSigners            = txInfoSignatories info
        !pid                  = ownCurrencySymbol context
        !mintedValues         = Value.flattenValue (txInfoMint info)
        !prefix_100           = createBuiltinByteString [0,6,67,176]       -- (100) is "000643b0"
        !prefix_222           = createBuiltinByteString [0,13,225,64]      -- (222) is "000de140"
        !refName              = uniqueTokenName prefix_100 firstTx
        !nftName              = uniqueTokenName prefix_222 firstTx
    in (traceIfFalse "val" $ Value.valueOf refValue lockPid lockTkn == 1)  -- check if correct reference
    && (traceIfFalse "ref" $ checkAllMints mintedValues pid refName)       -- must mint ref token
    && (traceIfFalse "nft" $ checkAllMints mintedValues pid nftName)       -- must mint nft
    && (traceIfFalse "idx" $ V2.txOutRefIdx firstTx < 256)                 -- prevent roll over collision
    && (traceIfFalse "sig" $ signedBy txSigners hotPkh)                    -- hot key must sign
  where
    checkAllMints :: [(V2.CurrencySymbol, V2.TokenName, Integer)] -> V2.CurrencySymbol -> V2.TokenName -> Bool
    checkAllMints []                  _   _    = traceError "Nothing Minted"
    checkAllMints ((cs, tkn, amt):xs) cs' tkn' = 
      if cs == cs' && tkn == tkn' && amt == (1 :: Integer) -- the correct mint
        then True                                          -- found the mint
        else checkAllMints xs cs' tkn'                     -- keep searching

    getReferenceDatum :: V2.TxOut -> ReferenceDatum
    getReferenceDatum x = 
      case V2.txOutDatum x of
        V2.NoOutputDatum              -> traceError "No Datum"
        (V2.OutputDatumHash _)        -> traceError "Embedded Datum"
        (V2.OutputDatum (V2.Datum d)) -> 
          case PlutusTx.fromBuiltinData d of
            Nothing     -> traceError "Bad Data"
            Just inline -> PlutusTx.unsafeFromBuiltinData @ReferenceDatum inline
-------------------------------------------------------------------------------
wrappedPolicy :: ScriptParameters -> BuiltinData -> BuiltinData -> ()
wrappedPolicy s x y = check (mkPolicy s (V2.unsafeFromBuiltinData x) (V2.unsafeFromBuiltinData y))

policy :: ScriptParameters -> V2.MintingPolicy
policy sp = V2.mkMintingPolicyScript $ 
  $$(PlutusTx.compile [|| wrappedPolicy ||])
  `PlutusTx.applyCode`
  PlutusTx.liftCode sp

mintingPlutusScript :: ScriptParameters -> PlutusScript PlutusScriptV2
mintingPlutusScript sp = PlutusScriptSerialised . SBS.toShort $ LBS.toStrict $ 
    serialise $ Plutonomy.optimizeUPLCWith theOptimizerOptions $ 
    V2.unMintingPolicyScript (policy sp)