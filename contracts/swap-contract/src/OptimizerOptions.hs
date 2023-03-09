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
module OptimizerOptions
  ( theOptimizerOptions
  ) where
import PlutusTx.Prelude
import Plutonomy.Raw.Transform as Options
import Plutonomy


-- | custom optimizer options; change as needed
theOptimizerOptions :: Plutonomy.OptimizerOptions
theOptimizerOptions = Plutonomy.OptimizerOptions
  { ooOptimizerRounds = 2
  , ooPreInlineConsts = True
  , ooInlineUsedOnce  = True
  , ooInlineSaturated = True
  , ooSplitDelay      = True
  , ooEtaForce        = True
  , ooEtaFun          = True
  , ooFloatOutLambda  = True
  , ooFloatOutDelay   = True
  , ooFloatOutAppArg  = Just Options.FloatOutAppArgValue
  , ooIfLambda        = True
  , ooCombineBindings = True
  , ooKnownRewrites   = True
  , ooTraceRewrite    = Just TraceRewrite -- Just TraceRemove
  , ooIfeRewrite      = Just IfeRewriteMore
  , ooAppError        = Just Options.AppErrorAll
  , ooCommuteEquals   = True
  , ooLetZero         = True
  , ooCSE             = True
  , ooFloatIn         = True
  }