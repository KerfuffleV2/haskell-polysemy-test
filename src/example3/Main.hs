{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE TemplateHaskell  #-}
module Main where

{- NOTE: The changes in this version aren't my own. Please see https://github.com/KerfuffleV2/haskell-polysemy-test/pull/1 -}


import Polysemy
import Polysemy.Input
import Polysemy.Output
import Polysemy.State
import Polysemy.Trace
import System.Random (newStdGen, randomRs, randomRIO)
import Text.Read (readMaybe)


newtype GuessConfig = GuessConfig { unGuessConfig :: (Int, (Int, Int)) }
  deriving Show

runListInputForever :: [i] -> Sem (Input i ': r) a -> Sem r a
runListInputForever is = fmap snd . runState (cycle is) . reinterpret \case
  Input -> do
    gotten <- get
    let (s : ss) = gotten  -- safe because the state is infinite
    put ss
    pure s


tracePrefix :: Member Trace r => Sem r a -> Sem r a
tracePrefix = intercept \case
  Trace msg -> trace $ "> " ++ msg



guessProg :: forall r. Members '[Input String, Input Int, Trace] r => GuessConfig -> Sem r [Bool]
guessProg config = go True []
  where
    -- Note: Because of ScopedTypeVariables, r and its constraints are available in the function body.
    go :: Bool -> [Bool] -> Sem r [Bool]
    go False record = return $ reverse record
    go True record = do
      let GuessConfig (guessesallowed, (minguess, maxguess)) = config
      trace ""
      trace $ "I'm thinking of a number between " <> show minguess <> " and " <> show maxguess <> ". Guesses allowed: " <> show guessesallowed
      correct <- input
      let
        doGuess :: Int -> Sem r (Maybe Int)
        doGuess remaining = do
          let guessnum = (guessesallowed - remaining) + 1
          trace $ "Enter guess #" <> show guessnum <> ":"
          maybeguess <- readMaybe <$> input
          case maybeguess of
            Just guess
              | guess > correct -> trace $ show guess <> " is too high."
              | guess < correct -> trace $ show guess <> " is too low."
              | otherwise -> return ()
            Nothing -> trace "That's not a valid number. You just wasted a guess!"
          return maybeguess

        doGuesses :: Int -> Sem r Bool
        doGuesses (-1) = trace "You win!" >> return True
        doGuesses 0 = do
          trace $ "You ran out of guesses. Game over! The number was " <> show correct <> "."
          return False
        doGuesses remaining = do
          maybeguess <- doGuess remaining
          doGuesses $ if maybeguess == Just correct then (-1) else remaining - 1

        playAgain :: Sem r Bool
        playAgain = do
          trace ""
          trace "Play again? (Y/n)"
          (`elem` ["y", "yes", "hokay", ""]) <$> input

      won <- doGuesses guessesallowed
      again <- playAgain
      trace $ if again then "Starting a new game!" else "Goodbye!"
      go again (won:record)


main :: IO ()
main = do
  rgen <- newStdGen
  let
    guessesallowed = 5
    guessmin = 1
    guessmax = 100
    bounds = (guessmin, guessmax)
    conf = GuessConfig (guessesallowed, bounds)
    randlist = randomRs (guessmin, guessmax) rgen
    inputlist = ["50", "75", "", "", "", "y","75"]
    (progoutput :: [String], progreturn)
            = run
            . runFoldMapOutput (:[])
            . runTraceAsOutput
            . tracePrefix
            . runListInputForever randlist
            . runListInputForever inputlist
            $ guessProg conf

  putStrLn "====== Result from pure program"
  mapM_ print progoutput
  putStrLn $ "Successful completion with result: " <> show progreturn
  putStrLn "\n"
  putStrLn "====== Running IO program"
  iresult <- runM
           . runTraceIO
           . tracePrefix
           . runMonadicInput (sendM $ randomRIO bounds)
           . runMonadicInput (sendM getLine)
           $ guessProg conf
  putStrLn "====== Result from IO program"
  putStrLn $ "Completion with result: " <> show iresult
