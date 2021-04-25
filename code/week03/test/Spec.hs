{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import PlutusTx.Prelude hiding (Applicative (..), Semigroup (..), return, (<$>), (>>), (>>=))
import Test.Tasty
import Test.Tasty.Hedgehog (HedgehogTestLimit (..))
import qualified Homework1 as Homework1

main :: IO ()
main = defaultMain tests

-- | Number of successful tests for each hedgehog property.
--   The default is 100 but we use a smaller number here in order to speed up
--   the test suite.
limit :: HedgehogTestLimit
limit = HedgehogTestLimit (Just 5)

tests :: TestTree
tests =
  localOption limit $
    testGroup
      "tests"
      [ Homework1.tests
      ]

