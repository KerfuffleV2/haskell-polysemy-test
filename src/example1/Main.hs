{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE TemplateHaskell  #-}
module Main where

import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Output
import Polysemy.Reader
import Polysemy.State
import Prelude hiding (readLn)
import System.Random (newStdGen, randomRs)
import Text.Read (readMaybe)


newtype RandState = RandState { unRandState :: [Int] }
  deriving Show

newtype GuessConfig = GuessConfig { unGuessConfig :: (Int, (Int, Int)) }
  deriving Show

data Guess m a where
  ReadLn :: Guess m String
  WriteLn :: String -> Guess m ()
  GetConfig :: Guess m GuessConfig
  GetNextRand :: Guess m Int

makeSem ''Guess


runGuessIO :: Members '[Reader GuessConfig, State RandState, Lift IO] r => Sem (Guess ': r) a -> Sem r a
runGuessIO = interpret \case
  ReadLn -> sendM getLine
  WriteLn s -> sendM . putStrLn $ '>':' ':s
  GetConfig -> ask
  GetNextRand -> do
    (h,rest) <- gets $ \(RandState (h:rest)) -> (h,rest)
    put $ RandState rest
    return h


runGuessPure :: Members '[Reader GuessConfig, State RandState] r => [String] -> Sem (Guess ': r) a -> Sem r ([String], Either String a)
runGuessPure inp = runFoldMapOutput pure . runListInput inp . runError . reinterpret3 \case
  ReadLn -> input >>= maybe (throw "Failed to read") return
  WriteLn s -> output $ '>':' ':s
  GetConfig -> ask
  GetNextRand -> do
    (h,rest) <- gets $ \(RandState (h:rest)) -> (h,rest)
    put $ RandState rest
    return h


guessProg :: forall r. Member Guess r => Sem r [Bool]
guessProg = go True []
  where
    -- Note: Because of ScopedTypeVariables, r and its constraints are available in the function body.
    go :: Bool -> [Bool] -> Sem r [Bool]
    go False record = return $ reverse record
    go True record = do
      GuessConfig (guessesallowed, (minguess, maxguess)) <- getConfig
      writeLn ""
      writeLn $ "I'm thinking of a number between " <> show minguess <> " and " <> show maxguess <> ". Guesses allowed: " <> show guessesallowed
      correct <- getNextRand
      let
        doGuess :: Int -> Sem r (Maybe Int)
        doGuess remaining = do
          let guessnum = (guessesallowed - remaining) + 1
          writeLn $ "Enter guess #" <> show guessnum <> ":"
          maybeguess <- readMaybe <$> readLn
          case maybeguess of
            Just guess
              | guess > correct -> writeLn $ show guess <> " is too high."
              | guess < correct -> writeLn $ show guess <> " is too low."
              | otherwise -> return ()
            Nothing -> writeLn "That's not a valid number. You just wasted a guess!"
          return maybeguess

        doGuesses :: Int -> Sem r Bool
        doGuesses (-1) = writeLn "You win!" >> return True
        doGuesses 0 = do
          writeLn $ "You ran out of guesses. Game over! The number was " <> show correct <> "."
          return False
        doGuesses remaining = do
          maybeguess <- doGuess remaining
          doGuesses $ if maybeguess == Just correct then (-1) else remaining - 1

        playAgain :: Sem r Bool
        playAgain = do
          writeLn ""
          writeLn "Play again? (Y/n)"
          (`elem` ["y", "yes", "hokay", ""]) <$> readLn

      won <- doGuesses guessesallowed
      again <- playAgain
      writeLn $ if again then "Starting a new game!" else "Goodbye!"
      go again (won:record)


main :: IO ()
main = do
  rgen <- newStdGen
  let
    guessesallowed = 5
    guessmin = 1
    guessmax = 100
    conf = GuessConfig (guessesallowed, (guessmin, guessmax))
    randlist = RandState $ randomRs (guessmin, guessmax) rgen
    inputlist = ["50", "75", "", "", "", "y","75"]
    presult :: (RandState, ([String], Either String [Bool]))
    presult = run . runReader conf . runState randlist $ runGuessPure inputlist guessProg

  putStrLn "====== Result from pure program"
  case snd presult of
    (progoutput, Left err) -> do
      mapM_ print progoutput
      putStrLn $ "Completed with error: " <> err
    (progoutput, Right progreturn) -> do
      mapM_ print progoutput
      putStrLn $ "Successful completion with result: " <> show progreturn
  putStrLn "\n"
  putStrLn "====== Running IO program"
  iresult <- fmap snd . runM . runReader conf . runState randlist $ runGuessIO guessProg
  putStrLn "====== Result from IO program"
  putStrLn $ "Completion with result: " <> show iresult
