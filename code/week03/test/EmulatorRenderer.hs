{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module EmulatorRenderer where

import qualified Data.Aeson as A
import Data.Default (Default (def))
import Data.Text.Prettyprint.Doc (Pretty (..), defaultLayoutOptions, layoutPretty)
import Data.Text.Prettyprint.Doc.Render.String (renderString)
import Plutus.Trace.Emulator (EmulatorTrace, runEmulatorTraceIO', showEvent)
import Plutus.Trace.Emulator.Types
  ( ContractInstanceLog (ContractInstanceLog),
    ContractInstanceMsg (ContractLog, CurrentRequests, HandledRequest, NoRequestsHandled, StoppedWithError),
    UserThreadMsg (UserLog),
  )
import PlutusTx.Prelude (IO, Maybe (..), Semigroup ((<>)), Show (show), String, ($), (.))
import Wallet.Emulator.MultiAgent (EmulatorEvent' (..))

-- Print out the log.
printEmulatorLog :: EmulatorTrace () -> IO ()
printEmulatorLog = runEmulatorTraceIO' def {showEvent = testShowEvent} def

-- Customization of default EmulatorEvent' show function `Plutus.Trace.Emulator.defaultShowEvent`.
-- Allows to get more insight if required.
testShowEvent :: EmulatorEvent' -> Maybe String
testShowEvent = \case
  UserThreadEvent (UserLog msg) -> Just $ "*** USER LOG: " <> msg
  InstanceEvent (ContractInstanceLog (ContractLog (A.String msg)) _ _) -> Just $ "*** CONTRACT LOG: " <> show msg
  InstanceEvent (ContractInstanceLog (StoppedWithError err) _ _) -> Just $ "*** CONTRACT STOPPED WITH ERROR: " <> show err
  InstanceEvent (ContractInstanceLog NoRequestsHandled _ _) -> Nothing
  InstanceEvent (ContractInstanceLog (HandledRequest _) _ _) -> Nothing
  InstanceEvent (ContractInstanceLog (CurrentRequests _) _ _) -> Nothing
  SchedulerEvent _ -> Nothing
  ChainIndexEvent _ _ -> Nothing
  WalletEvent _ _ -> Nothing
  ev -> Just . renderString . layoutPretty defaultLayoutOptions . pretty $ ev
