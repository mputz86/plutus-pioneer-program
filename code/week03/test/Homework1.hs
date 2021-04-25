{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Homework1 where

import Control.Monad (void)
import Ledger
import qualified Ledger.Ada as Ada
import Plutus.Contract.Test
import Plutus.Trace.Emulator
import qualified Plutus.Trace.Emulator as Trace
import PlutusTx.Prelude hiding (Applicative (..), Semigroup (..), return, trace, (<$>), (>>), (>>=))
import Test.Tasty
import Week03.Homework1

w1, w2, w3 :: Wallet
w1 = Wallet 1
w2 = Wallet 2
w3 = Wallet 3

walletToPubKeyHash :: Wallet -> PubKeyHash
walletToPubKeyHash = pubKeyHash . walletPubKey

tests :: TestTree
tests =
  let giveAway :: Integer
      giveAway = 100
      deadline :: Slot
      deadline = 20
   in testGroup
        "homework week 03.1"
        [ checkPredicate
            "example from lecture (by Lars)"
            ( walletFundsChange w1 (Ada.lovelaceValueOf (-500))
                .&&. walletFundsChange w2 (Ada.lovelaceValueOf 500)
                .&&. assertNoFailedTransactions
            ) testCaseLecture,
          checkPredicate
            "giver can get back his money after deadline"
            ( walletFundsChange w1 mempty
                .&&. assertNoFailedTransactions
            )
            $ do
              _ <- callGive w1 w2 deadline giveAway
              void $ Trace.waitUntilSlot (deadline + 1)
              callGrab w1
              void $ Trace.waitNSlots 1,
          checkPredicate
            "grabber can get back money before deadline"
            ( walletFundsChange w1 (Ada.lovelaceValueOf (- giveAway))
                .&&. walletFundsChange w2 (Ada.lovelaceValueOf giveAway)
                .&&. assertNoFailedTransactions
            )
            $ do
              _ <- callGive w1 w2 deadline giveAway
              void $ Trace.waitUntilSlot (deadline - 1)
              callGrab w2
              void $ Trace.waitNSlots 1,
          checkPredicate
            "grabber can not get back money after deadline"
            ( walletFundsChange w1 (Ada.lovelaceValueOf (- giveAway))
                .&&. walletFundsChange w2 mempty
                .&&. assertNoFailedTransactions
            )
            $ do
              _ <- callGive w1 w2 deadline giveAway
              void $ Trace.waitUntilSlot (deadline + 1)
              callGrab w2
              void $ Trace.waitNSlots 1,
          checkPredicate
            "no other wallet can grab ada before or after deadline"
            ( walletFundsChange w1 (Ada.lovelaceValueOf (- giveAway))
                .&&. walletFundsChange w3 mempty
                .&&. assertNoFailedTransactions
            )
            $ do
              _ <- callGive w1 w2 deadline giveAway
              void $ Trace.waitUntilSlot (deadline -1)
              callGrab w3
              void $ Trace.waitUntilSlot (deadline + 1)
              callGrab w3
              void $ Trace.waitNSlots 1
        ]

testCaseLecture :: EmulatorTrace ()
testCaseLecture = do
  _ <- callGive w1 w2 10 500
  _ <- callGive w2 w1 5 500
  void $ Trace.waitUntilSlot 6
  callGrab w2
  void $ Trace.waitNSlots 1

callGive :: Wallet -> Wallet -> Slot -> Integer -> EmulatorTrace ()
callGive giver receiver deadline amount = do
  hdl <- Trace.activateContractWallet giver endpoints
  let receiverPubKeyHash = walletToPubKeyHash receiver
      params = GiveParams receiverPubKeyHash deadline amount
  Trace.callEndpoint @"give" hdl params

callGrab :: Wallet -> EmulatorTrace ()
callGrab grabber = do
  hdl <- Trace.activateContractWallet grabber endpoints
  Trace.callEndpoint @"grab" hdl ()
