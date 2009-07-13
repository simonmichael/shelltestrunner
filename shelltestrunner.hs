#!/usr/bin/env runhaskell
{-

shelltestrunner - a handy tool for testing command-line programs.

See shelltestrunner.cabal.

(c) Simon Michael 2009, released under GNU GPLv3

-}

module Main
where
import Control.Concurrent (forkIO)
import Control.Monad (liftM,when)
import Data.List (partition)
import Data.Maybe (fromJust,isJust,maybe)
import qualified Test.HUnit (Test)
import System.Console.ParseArgs hiding (args)
import System.Environment (withArgs)
import System.Exit (ExitCode(..),exitWith)
import System.IO (hGetContents, hPutStr, stderr)
import System.Process (runInteractiveCommand, waitForProcess)
import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit hiding (Test)
import Text.ParserCombinators.Parsec
import Text.Printf (printf)
import Text.RegexPR
import Debug.Trace
strace :: Show a => a -> a
strace a = trace (show a) a


version :: String
version = "0.3" -- sync with .cabal

data ArgId = HelpFlag
           | VersionFlag
           | DebugFlag
           | ImplicitTestsFlag
           | ExecutableArg
             deriving (Ord, Eq, Show)

argspec :: [Arg ArgId]
argspec = [
  Arg {argIndex = HelpFlag,
       argName  = Just "help",
       argAbbr  = Just 'h',
       argData  = Nothing,
       argDesc  = "show help"
      }
 ,Arg {argIndex = VersionFlag,
       argName  = Just "version",
       argAbbr  = Just 'V',
       argData  = Nothing,
       argDesc  = "show version"
      }
 ,Arg {argIndex = DebugFlag,
       argName  = Just "debug",
       argAbbr  = Just 'd',
       argData  = Nothing,
       argDesc  = "show verbose debugging output"
      }
 ,Arg {argIndex = ImplicitTestsFlag,
       argName  = Just "implicit-tests",
       argAbbr  = Just 'i',
       argData  = Nothing,
       argDesc  = "provide implicit tests for all omitted fields"
      }
 ,Arg {argIndex = ExecutableArg,
       argName  = Nothing,
       argAbbr  = Nothing,
       argData  = argDataRequired "executable" ArgtypeString,
       argDesc  = "executable program or shell command to test"
      }
 ]

data ShellTest = ShellTest {
     testname         :: String
    ,commandargs      :: String
    ,stdin            :: Maybe String
    ,stdoutExpected   :: Maybe Matcher
    ,stderrExpected   :: Maybe Matcher
    ,exitCodeExpected :: Maybe Matcher
    } deriving (Show)

type Regex = String

data Matcher = Exact String
             | Numeric String
             | PositiveRegex Regex
             | NegativeRegex Regex
               deriving (Show)

main :: IO ()
main = do
  args <- parseArgsIO ArgsTrailing argspec
  -- parseargs issue: exits at first gotArg if there's no exe argument
  when (args `gotArg` VersionFlag) printVersion
  when (args `gotArg` HelpFlag) $ printHelp args
  let (unprocessedopts, testfiles) = partition ((=="-").take 1) $ argsRest args
  when (args `gotArg` DebugFlag) $ do
         putStrLn $ "testing executable: " ++ (show $ fromJust $ getArgString args ExecutableArg)
         putStrLn $ "unprocessed options: " ++ show unprocessedopts
         putStrLn $ "test files: " ++ show testfiles
  shelltests <- mapM (parseShellTest args) testfiles
  withArgs unprocessedopts $ defaultMain $ concatMap (hUnitTestToTests.shellTestToHUnitTest args) shelltests

printVersion :: IO ()
printVersion = putStrLn version >> exitWith ExitSuccess

printHelp :: Args ArgId -> IO ()
printHelp args = putStrLn (argsUsage args) >> exitWith ExitSuccess

-- parsing

parseShellTest :: Args ArgId -> FilePath -> IO ShellTest
parseShellTest args f = do
  t <- liftM (either (error.show) id) $ parseFromFile shelltestp f
  when (args `gotArg` DebugFlag) $ do
    putStrLn $ "parsing file: " ++ f
    putStrLn $ "parsed test: " ++ show t
  return t

shelltestp :: Parser ShellTest
shelltestp = do
  st <- getParserState
  let f = sourceName $ statePos st
  many commentlinep
  c <- commandargsp
  i <- optionMaybe inputp
  o <- optionMaybe expectedoutputp
  e <- optionMaybe expectederrorp
  x <- optionMaybe expectedexitcodep
  return ShellTest{testname=f,commandargs=c,stdin=i,stdoutExpected=o,stderrExpected=e,exitCodeExpected=x}

linep,commentlinep,commandargsp,delimiterp,inputp,whitespacep :: Parser String

linep = anyChar `manyTill` newline

commentlinep = char '#' >> anyChar `manyTill` newline

commandargsp = linep

delimiterp = choice [try $ string "<<<", try $ string ">>>", (eof >> return "")]

inputp = string "<<<\n" >> (liftM unlines) (linep `manyTill` (lookAhead delimiterp))

expectedoutputp :: Parser Matcher
expectedoutputp = try $ do
                    string ">>>" >> optional (char '1')
                    whitespacep
                    choice [positiveregexmatcherp, negativeregexmatcherp, datalinesp]

expectederrorp :: Parser Matcher
expectederrorp = try $ do
                    string ">>>2"
                    whitespacep
                    choice [positiveregexmatcherp, negativeregexmatcherp, datalinesp]

expectedexitcodep :: Parser Matcher
expectedexitcodep = do
                      string ">>>="
                      whitespacep
                      choice [positiveregexmatcherp, negativeregexmatcherp, numericdatalinesp]

negativeregexmatcherp :: Parser Matcher
negativeregexmatcherp = do
  char '!'
  PositiveRegex r <- positiveregexmatcherp
  return $ NegativeRegex r

positiveregexmatcherp :: Parser Matcher
positiveregexmatcherp = do
  char '/'
  r <- many $ noneOf "/"
  char '/'
  whitespacep
  newline
  return $ PositiveRegex r

datalinesp :: Parser Matcher
datalinesp = do
  whitespacep
  newline
  (liftM $ Exact . unlines) (linep `manyTill` (lookAhead delimiterp))

numericdatalinesp :: Parser Matcher
numericdatalinesp = do
  whitespacep
  newline
  whitespacep
  s <- many1 $ oneOf "0123456789"
  newline
  return $ Numeric s

whitespacep = many $ oneOf " \t"

-- running

shellTestToHUnitTest :: Args ArgId -> ShellTest -> Test.HUnit.Test
shellTestToHUnitTest args t = testname t ~: do {r <- runShellTest args t; assertBool "" r}

runShellTest :: Args ArgId -> ShellTest -> IO Bool
runShellTest args ShellTest{testname=n,commandargs=c,stdin=i,stdoutExpected=o_expected,
                            stderrExpected=e_expected,exitCodeExpected=x_expected} = do
  let exe = fromJust $ getArgString args ExecutableArg
      cmd = unwords [exe,c]
      (o_expected',e_expected',x_expected') =
          case (args `gotArg` ImplicitTestsFlag) of
            True  -> (case o_expected of
                       Just m -> Just m
                       _ -> Just $ Exact ""
                    ,case e_expected of Just m -> Just m
                                        _      -> Just $ Exact  ""
                    ,case x_expected of Just m -> Just m
                                        _      -> Just $ Numeric "0")
            False -> (o_expected,e_expected,x_expected)
  when (args `gotArg` DebugFlag) $ do
    putStrLn $ "running test: " ++ n
    putStrLn $ "running command: " ++ cmd
  (ih,oh,eh,ph) <- runInteractiveCommand cmd
  when (isJust i) $ forkIO (hPutStr ih $ fromJust i) >> return ()
  o_actual <- hGetContents oh
  e_actual <- hGetContents eh
  -- force some evaluation here to avoid occasional waitForProcess hangs. cf http://hackage.haskell.org/trac/ghc/ticket/3369
  putStr $ printf "%d,%d" (length o_actual) (length e_actual)                                                                                                                  
  x_actual <- waitForProcess ph
  let o_ok = maybe True (o_actual `matches`) o_expected'
  let e_ok = maybe True (e_actual `matches`) e_expected'
  let x_ok = maybe True ((show $ fromExitCode x_actual) `matches`) x_expected'
  if o_ok && e_ok && x_ok
   then do
     return True 
   else do
     when (not o_ok) $ printExpectedActual "stdout" (fromJust o_expected') o_actual
     when (not e_ok) $ printExpectedActual "stderr" (fromJust e_expected') e_actual
     when (not x_ok) $ printExpectedActual "exit code" (fromJust x_expected') (show $ fromExitCode x_actual)
     return False

matches :: String -> Matcher -> Bool
matches s (PositiveRegex r) = s `containsRegex` r
matches s (NegativeRegex r) = not $ s `containsRegex` r
matches s (Numeric p)       = s == p
matches s (Exact p)         = s == p

showMatcher :: Matcher -> String
showMatcher (PositiveRegex r) = " /"++r++"/\n"
showMatcher (NegativeRegex r) = " !/"++r++"/\n"
showMatcher (Numeric s)       = "\n"++s
showMatcher (Exact s)         = "\n"++s

printExpectedActual :: String -> Matcher -> String -> IO ()
printExpectedActual f e a = hPutStr stderr $ printf "**Expected %s:%s**Got %s:\n%s" f (showMatcher e) f a


-- utils

-- toExitCode :: Int -> ExitCode
-- toExitCode 0 = ExitSuccess
-- toExitCode n = ExitFailure n

fromExitCode :: ExitCode -> Int
fromExitCode ExitSuccess     = 0
fromExitCode (ExitFailure n) = n

strip,lstrip,rstrip,dropws :: String -> String
strip = lstrip . rstrip
lstrip = dropws
rstrip = reverse . dropws . reverse
dropws = dropWhile (`elem` " \t")

containsRegex :: String -> String -> Bool
containsRegex s r = case matchRegexPR (""++r) s of
                      Just _ -> True
                      _ -> False
