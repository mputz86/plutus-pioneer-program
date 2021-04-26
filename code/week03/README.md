
# Testing with Emulator

## Write tests

- See `./test/Homework1.hs` for how to write tests with the `Emulator`
- In principle
  - Define what the expected change in the wallet is (predicate). Like: 100 more ADA.
  - Write the flow of actions similar to how it is done in playground. With the following primitives:
    - Calling contract endpoints and
    - "Waiting until" or "for" an amount of slots (between contract actions)
  - Run tests it with `cabal test`

- Example snipped (see `test/Homework1` for how to embed the following code properly in your test suite):
```haskell
let giveAway :: Integer
    giveAway = 100
    deadline :: Slot
    deadline = 20
 in checkPredicate
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
```


## Interactive testing in repl

- Launch repl with test-suite
```bash
cabal repl plutus-pioneer-program-week03-test
```
- Import `EmulatorRenderer` and test cases, like `Homework1`
```haskell
import EmulatorRenderer
import Homework1
```
- Run test case `testCaseLecture` (is of type `EmulatorTrace ()`) with logging
```haskell
printEmulatorLog testCaseLecture
```


## References

- `Crowdfunding` source code and tests in `plutus-use-cases`
