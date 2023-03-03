{-# LANGUAGE OverloadedStrings #-}

import           Control.Exception
import           Control.Monad
import           Control.Monad.ST
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Data.Array
import           Data.Array.ST
import           Data.Bits
import           Data.Char
import           Data.List
import           Data.List.Split
import           Data.Maybe
import qualified Data.Sequence as L
import           System.Console.GetOpt
import           System.Directory
import           System.Environment
import           System.Exit
import           System.IO
import           System.Process
import           System.Random
import qualified Text.Read as R

newtype Board = Board (Array (Int, Int) (Maybe Int))

data Difficulty = Easy | Medium | Hard | Insane
  deriving Enum

data Opt    = OptDifficulty Difficulty | OptHelp | OptError String
data Config = CF { cfDifficulty :: Maybe Difficulty
                 , cfHelp       :: Bool
                 , cfError      :: Maybe String }

emptyConfig :: Config
emptyConfig = CF Nothing False Nothing

toCount :: Difficulty -> Int
toCount Easy   = 36
toCount Medium = 30
toCount Hard   = 25
toCount Insane = 22

toConfig :: [Opt] -> Config
toConfig opts = case result of
  Left err -> emptyConfig { cfError = Just err }
  Right cf -> cf
  where
    result = runExcept $ foldM worker emptyConfig opts
    worker cf (OptDifficulty d)
      | Nothing <- cfDifficulty cf = pure cf { cfDifficulty = Just d }
      | otherwise                  = throwE "Arg Error: Multiple difficulty level provided"
    worker cf OptHelp           = pure $ cf { cfHelp = True }
    worker cf (OptError errMsg) = throwE errMsg

instance Show Board where
  show :: Board -> String
  show (Board b) = concatMap worker (assocs b)
    where
      worker ((r, c), mN) = pref ++ maybe "X" show mN
        where
          pref
            | r `elem` [4, 7] && c == 1 = "\n\n"
            | r > 1 && c == 1           = "\n"
            | c `elem` [4, 7]           = "  "
            | c > 1                     = " "
            | otherwise                 = ""

mkRandomBoard :: StdGen -> Board
mkRandomBoard gen = Board $ runST $ do
  arrST <- newArray ((1, 1), (9, 9)) Nothing :: ST s (STArray s (Int, Int) (Maybe Int))
  let worker gen n = do
        let (pos, gen') = randomR ((1, 1), (9, 9)) gen
        writeArray arrST pos (Just n)
        return gen'
  foldM_ worker gen [1..9]
  freeze arrST

options :: [OptDescr Opt]
options = [ Option "g" ["gen", "generate"] (OptArg calcDifficulty "easy")
                   "Generate a Sudoku puzzle. The difficulty levels can be 'easy', 'medium', 'hard', or 'insane'"
          , Option "E" ["easy"] (NoArg $ OptDifficulty Easy)
                   "Shortcut for generating an easy Sudoku puzzle"
          , Option "M" ["medium"] (NoArg $ OptDifficulty Medium)
                   "Shortcut for generating a medium Sudoku puzzle"
          , Option "H" ["hard"] (NoArg $ OptDifficulty Hard)
                   "Shortcut for generating a hard Sudoku puzzle"
          , Option "I" ["insane"] (NoArg $ OptDifficulty Insane)
                   "Shortcut for generating an insane Sudoku puzzle, it can take quite some time..."
          , Option "h" ["help"] (NoArg OptHelp)
                   "Prompt the help message" ]
  where
    calcDifficulty Nothing    = OptDifficulty Easy
    calcDifficulty (Just str) = case toLower <$> str of
      "e"      -> OptDifficulty Easy
      "easy"   -> OptDifficulty Easy
      "m"      -> OptDifficulty Medium
      "medium" -> OptDifficulty Medium
      "h"      -> OptDifficulty Hard
      "hard"   -> OptDifficulty Hard
      "i"      -> OptDifficulty Insane
      "insane" -> OptDifficulty Insane
      str      -> OptError $ "Arg Error: Unrecognised difficulty " ++ str

helpMsg :: String
helpMsg = unlines ["Usage:", "  ./Sudoku <Option> (<input.txt> | <output.txt>)", usageInfo "Options:" options]

main :: IO ()
main = void . runExceptT . handleErr $ do
  rawArgs <- lift getArgs
  let (opts, args, errs) = getOpt RequireOrder options rawArgs
  unless (null errs) $ throwE (unlines errs)
  let config             = toConfig opts
  case cfError config of
    Just err -> throwE err
    Nothing  -> do
      when (cfHelp config) $ lift (putStrLn helpMsg)
      case cfDifficulty config of
        Nothing -> case args of
          []       -> unless (cfHelp config) $ throwE "Please provide the Sudoku input!"
          [src]    -> do
            str     <- lift $ readFile src
            board   <- except $ parseBoard str
            mResult <- solve board
            lift $ case mResult of
              Nothing     -> putStrLn "No solution!"
              Just result -> print result
          _ : rems -> throwE ("Surplus arguments: " ++ show rems)
        Just d  -> case args of
          _ : _ : rems -> throwE ("Surplus arguments: " ++ show rems)
          _            -> do
            rawGen <- getStdGen
            let seed = fst $ random rawGen
            let gen  = mkStdGen seed
            lift $ putStrLn ("The seed is " ++ show seed ++ ".")
            let gen' = snd $ randomR (0, fromEnum d) gen
            mPuzzle <- genPuzzle gen (toCount d)
            case mPuzzle of
              Just puzzle -> lift $ case listToMaybe args of
                Nothing   -> print puzzle
                Just dest -> writeFile dest (show puzzle)
              Nothing     -> throwE "Generation failed due to internal error."

genPuzzle :: StdGen -> Int -> ExceptT String IO (Maybe Board)
genPuzzle gen count = do
  let startBoard = mkRandomBoard gen
  mSolution <- solve startBoard
  case mSolution of
    Nothing       -> pure Nothing
    Just solution -> do
      (file, handle) <- lift $ openTempFile "./" "mmzk"
      lift $ hClose handle
      let initPoz = L.fromList [(x, y) | x <- [1..9], y <- [1..9]]
      result <- worker (Just file) gen [(solution, initPoz)] 81
      lift $ removeFile file
      return result
  where
    worker file gen bpz@((Board b, poz) : bps) c
      | c == count = pure $ Just (Board b)
      | L.null poz = let (bc :: Int, gen') = random gen
                         backoff = min ((1 + countTrailingZeros bc) * 5) (81 - c)
                     in  worker file gen' (drop backoff bpz) (c + backoff)
      | otherwise  = do
        let (ix, gen') = randomR (0, length poz - 1) gen
        let pos        = poz `L.index` ix
        let poz'       = L.deleteAt ix poz
        let board'     = Board (b // [(pos, Nothing)])
        result <- callClingo3 file board' ["-n", "2"]
        case length result of
          1 -> worker file gen' ((board', poz') : bpz) (c - 1)
          _ -> worker file gen' ((Board b, poz') : bps) c
    worker _ _ [] _ = pure Nothing

callClingo3 :: Maybe FilePath -> Board -> [String] -> ExceptT String IO [String]
callClingo3 mPath (Board b) opts = do
  file <- case mPath of
    Nothing   -> do
      (file, handle) <- lift $ openTempFile "./" "mmzk"
      lift $ hClose handle
      return file
    Just path -> pure path
  lift $ writeFile file base
  forM_ (assocs b) $ \((r, c), mN) -> case mN of
    Nothing -> pure ()
    Just n  -> lift . appendFile file
              $ concat ["grid(", intercalate "," (show <$> [r, c, n]), ")."]
  (exitCode, result, err) <- lift $ readProcessWithExitCode "clingo3" (file : opts) ""
  case mPath of
    Nothing -> lift $ removeFile file
    Just _  -> pure ()
  case exitCode of
    ExitSuccess   -> throwE err
    ExitFailure n -> case isSat n of
      Nothing -> throwE err
      Just _  -> return (tail $ splitOn "Answer:" result)

solve :: Board -> ExceptT String IO (Maybe Board)
solve board = do
  results <- callClingo3 Nothing board []
  return $ case results of
    []           -> Nothing
    solution : _ -> Just (buildBoard $ parseResult solution)

base :: String
base = "numero(1..9).\n\
       \1 { grid(R, C, N) : numero(N) } 1 :- numero(R), numero(C).\n\
       \:- grid(R, C1, N), grid(R, C2, N), C1 != C2.\n\
       \:- grid(R1, C, N), grid(R2, C, N), R1 != R2.\n\
       \group(1..3, 1..3).\n\
       \group(4..6, 4..6).\n\
       \group(7..9, 7..9).\n\
       \:- grid(R1, C1, N), grid(R2, C2, N), group(R1, R2), group(C1, C2), (R1, C1) != (R2, C2).\n"

handleErr :: ExceptT String IO () -> ExceptT String IO ()
handleErr = flip catchE (lift . putStrLn . ("An error has occured when running Sudoku:\n" ++)) . mapExceptT (handle worker)
  where
    worker (e :: SomeException) = pure (Left (show e))

parseBoard :: String -> Either String Board
parseBoard str
  | length elems /= 81 = Left "Expect exactly 9 * 9 numbers!"
  | otherwise          = Board . listArray ((1, 1), (9, 9)) <$> mapM num elems
  where
    elems  = concatMap words (lines str)
    num []           = Left "Expecting either a number between 0 and 9 or 'X' for each grid!"
    num n'@(n : rem) = if null rem && (isDigit n || n == 'X')
        then Right (R.readMaybe [n])
        else Left $ "Invalid number '" <> n' <> "'! It should be either a number between 0 and 9 or 'X'."

parseResult :: String -> [(Int, Int, Int)]
parseResult str = mapMaybe worker (words str)
  where
  worker str
    | pref /= "grid" = Nothing
    | otherwise      = let [r, c, n] = splitOn "," (init (tail suff))
                       in  Just (read r, read c, read n)
    where
      (pref, suff) = splitAt 4 str

buildBoard :: [(Int, Int, Int)] -> Board
buildBoard grids = Board $ runST $ do
  arrST <- newArray ((1, 1), (9, 9)) Nothing :: ST s (STArray s (Int, Int) (Maybe Int))
  forM_ grids $ \(r, c, n) -> do
    writeArray arrST (r, c) (Just n)
  freeze arrST

isSat :: Int -> Maybe Bool
isSat n
  | n == 10 || n == 30 = Just True
  | n == 20            = Just False
  | otherwise          = Nothing
