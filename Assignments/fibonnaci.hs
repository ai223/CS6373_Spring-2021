module FibStateful
 (
 FibState (..),
 FibStateful,
 getPreviousNumber,
 setPreviousNumber,
 getPreviousPreviousNumber,
 setPreviousPreviousNumber,
 getCurrentNumber,
 setCurrentNumber,
 ) where
-- |The data type containing all the state that we need during our calculation
data FibState = FibState {
 previousNumber :: Integer,
 previousPreviousNumber :: Integer,
 currentNumber :: Integer
} deriving (Show)

-- A function that takes state and produces a new, updated state, along with its normal return value
type FibStateful a = FibState -> (FibState, a)

------------

-- Some helper functions that make it easier to access that state. These functions are getters and setters.
-- The setters effectively act like assignment operators by mutating state! Note that the empty tuple ()
-- means that the function doesn't produce a useful result, it only updates the state; () is therefore like void.

-- For each getter function, we just extract the appropriate value from the old state and return it, along
-- with the the unchanged state.

-- For each setter, we return a modified state, which is the same as the old state, except for the one field
-- that we're updating. We also return () to indicate that the function doesn't produce a useful value.

getPreviousNumber :: FibStateful Integer
getPreviousNumber oldstate = (oldstate, previousNumber oldstate)

setPreviousNumber :: Integer -> FibStateful ()
setPreviousNumber newval oldstate =
 (FibState {previousNumber = newval,
            previousPreviousNumber = previousPreviousNumber oldstate,
            currentNumber = currentNumber oldstate}, ())

getPreviousPreviousNumber :: FibStateful Integer
getPreviousPreviousNumber oldstate = (oldstate, previousPreviousNumber oldstate)

setPreviousPreviousNumber :: Integer -> FibStateful ()
setPreviousPreviousNumber newval oldstate =
 (oldstate {previousPreviousNumber = newval,
            previousNumber = previousNumber oldstate,
            currentNumber = currentNumber oldstate}, () )

getCurrentNumber :: FibStateful Integer
getCurrentNumber oldstate = (oldstate, currentNumber oldstate)

setCurrentNumber :: Integer -> FibStateful ()
setCurrentNumber newval oldstate = (oldstate {currentNumber = newval,
                                              previousPreviousNumber = previousPreviousNumber oldstate,
                                              previousNumber = previousNumber oldstate}, ())

-- my solution

fibonnaci :: Integer -> Integer
fibonnaci x
  | x < 1 = 0
  | x < 3 = 1
  | otherwise = snd (getCurrentNumber (fibonnaciHelper (FibState 0 1 0) x))

fibonnaciHelper :: FibState -> Integer -> FibState
fibonnaciHelper state 0 = state
fibonnaciHelper state x = fibonnaciHelper (applyFibCalc state) (x - 1)

applyFibCalc :: FibState -> FibState
applyFibCalc state = do
  let num1 = snd (getPreviousPreviousNumber state)
  let num2 = snd (getPreviousNumber state)
  let curr = num1 + num2
  let state' = fst (setPreviousPreviousNumber num2 state)
  let state'' = fst (setPreviousNumber curr state')
  let stateUpdated = fst (setCurrentNumber curr state'')
  stateUpdated
