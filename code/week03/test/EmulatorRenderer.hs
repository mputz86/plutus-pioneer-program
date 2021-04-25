{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module EmulatorRenderer where

import qualified Control.Foldl as L
import Control.Monad.Freer (run)
import Control.Monad.Freer.Extras.Log (LogLevel (..))
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Default (Default (..))
import qualified Data.Text.Encoding as T
import Data.Text.Prettyprint.Doc (Pretty (..), defaultLayoutOptions, layoutPretty, vsep)
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import Plutus.Trace.Emulator
import qualified Plutus.Trace.Emulator as Trace
import PlutusTx.Prelude hiding (Applicative (..), Semigroup (..), return, trace, (<$>), (>>), (>>=))
import qualified Streaming.Prelude as S
import System.IO
import qualified Wallet.Emulator.Folds as Folds
import Wallet.Emulator.Stream (filterLogLevel, foldEmulatorStreamM)

printEmulatorLog :: EmulatorTrace () -> IO ()
printEmulatorLog = BSL.hPutStrLn stdout . renderEmulatorLog

-- Copied from Spec.Crowdfunding in plutus-use-cases
renderEmulatorLog :: EmulatorTrace () -> BSL.ByteString
renderEmulatorLog trace =
  let result =
        run $
          foldEmulatorStreamM (L.generalize Folds.emulatorLog) $
            filterLogLevel Info $
              Trace.runEmulatorStream def trace
   in BSL.fromStrict $ T.encodeUtf8 $ renderStrict $ layoutPretty defaultLayoutOptions $ vsep $ fmap pretty $ S.fst' result
