#!/usr/bin/env runhaskell
{-# LANGUAGE DeriveDataTypeable #-}
{-

shelltestrunner - a handy tool for testing command-line programs.

See shelltestrunner.cabal.

(c) Simon Michael 2009, released under GNU GPLv3

-}

module Main
where
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (liftM,when)
import Data.List (intercalate)
import Data.Maybe (fromJust,isJust,maybe,isNothing,catMaybes)
import qualified Test.HUnit (Test)
import System.Console.CmdArgs hiding (args)
import qualified System.Console.CmdArgs as CmdArgs (args)
import System.Environment (withArgs)
import System.Exit (ExitCode(..))
import System.IO (Handle, hGetContents, hPutStr)
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


version, progname, prognameandversion :: String
version = "0.6.98" -- keep synced with .cabal file
progname = "shelltestrunner"
prognameandversion = progname ++ " " ++ version

data Args = Args {
--     debug :: Bool
     implicittests :: Bool
    ,executable :: String
    ,testfiles :: [String]
    ,otheropts :: [String]
    } deriving (Show, Data, Typeable)

argsmode :: Mode Args
argsmode = mode $ Args{
--            debug=def &= explicit & flag "debug" & text "debug verbosity"
            implicittests=def &= text "provide implicit tests for all omitted fields"
           ,executable=def &= argPos 0 & typ "EXE" & text "executable under test"
           ,testfiles=def &= CmdArgs.args & typ "TESTFILES" & text "test files"
           ,otheropts=def &= unknownFlags & explicit & typ "FLAGS" & text "other flags are passed to test runner"
           }

data ShellTest = ShellTest {
     testname         :: String
    ,commandargs      :: String
    ,stdin            :: Maybe String
    ,stdoutExpected   :: Maybe Matcher
    ,stderrExpected   :: Maybe Matcher
    ,exitCodeExpected :: Maybe Matcher
    } deriving (Show)

type Regex = String

data Matcher = Lines String
             | Numeric String
             | PositiveRegex Regex
             | NegativeRegex Regex
               deriving (Show)

main :: IO ()
main = do
  args <- cmdArgs prognameandversion [argsmode]
  loud <- isLoud
  when loud $ do
         putStrLn $ "testing executable: " ++ (show $ executable args)
         putStrLn $ "test files: " ++ (show $ testfiles args)
         putStrLn $ "test runner options: " ++ (show $ otheropts args)
  shelltests <- liftM concat $ mapM parseShellTestFile (testfiles args)
  withArgs (otheropts args) $ defaultMain $ concatMap (hUnitTestToTests.shellTestToHUnitTest args) shelltests

-- parsing

parseShellTestFile :: FilePath -> IO [ShellTest]
parseShellTestFile f = do
  loud <- isLoud
  when loud $ putStrLn $ "parsing file: " ++ f
  ts <- liftM (either (error.show) id) $ parseFromFile (many shelltestp) f
  let ts' | length ts > 1 = [t{testname=testname t++":"++show n} | (n,t) <- zip ([1..]::[Int]) ts]
          | otherwise     = ts
  when loud $ putStrLn $ "parsed tests: " ++ show ts'
  return ts'

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
  when (null c && (isNothing i) && (null $ catMaybes [o,e,x])) $ fail "empty test file"
  return ShellTest{testname=f,commandargs=c,stdin=i,stdoutExpected=o,stderrExpected=e,exitCodeExpected=x}

linep,commentlinep,commandargsp,delimiterp,inputp,whitespacep :: Parser String

linep = anyChar `manyTill` newline

commentlinep = char '#' >> anyChar `manyTill` newline

commandargsp = linep

delimiterp = choice [try $ string "<<<", try $ string ">>>", (eof >> return "")]

inputp = string "<<<" >> newline >> (liftM unlines) (linep `manyTill` (lookAhead delimiterp))

expectedoutputp :: Parser Matcher
expectedoutputp = try $ do
  string ">>>" >> optional (char '1')
  whitespacep
  choice [positiveregexmatcherp, negativeregexmatcherp, linesmatcherp]

expectederrorp :: Parser Matcher
expectederrorp = try $ do
  string ">>>2"
  whitespacep
  choice [positiveregexmatcherp, negativeregexmatcherp, linesmatcherp]

expectedexitcodep :: Parser Matcher
expectedexitcodep = do
  string ">>>="
  whitespacep
  choice [positiveregexmatcherp, negativeregexmatcherp, numericmatcherp]

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

linesmatcherp :: Parser Matcher
linesmatcherp = do
  whitespacep
  newline
  (liftM $ Lines . unlines) (linep `manyTill` (lookAhead delimiterp))

numericmatcherp :: Parser Matcher
numericmatcherp = do
  s <- many1 $ oneOf "0123456789"
  whitespacep
  newline
  return $ Numeric s

whitespacep = many $ oneOf " \t"

-- running

shellTestToHUnitTest :: Args -> ShellTest -> Test.HUnit.Test
shellTestToHUnitTest args ShellTest{testname=n,commandargs=c,stdin=i,stdoutExpected=o_expected,
                                    stderrExpected=e_expected,exitCodeExpected=x_expected} = 
 n ~: do
  let exe = executable args
      cmd = unwords [exe,c]
      (o_expected',e_expected',x_expected') =
          case (implicittests args)
          of
            True  -> (case o_expected of
                       Just m -> Just m
                       _ -> Just $ Lines ""
                    ,case e_expected of Just m -> Just m
                                        _      -> Just $ Lines  ""
                    ,case x_expected of Just m -> Just m
                                        _      -> Just $ Numeric "0")
            False -> (o_expected,e_expected,x_expected)
  loud <- isLoud
  when loud $ do
    putStrLn $ "running test: " ++ n
    putStrLn $ "running command: " ++ cmd
  (o_actual, e_actual, x_actual) <- runCommandWithInput cmd i
  if (x_actual == 127) -- catch bad executable - should work on posix systems at least
   then ioError $ userError e_actual -- XXX still a test failure; should be an error
   else assertString $ addnewline $ intercalate "\n" $ filter (not . null) [
             if (maybe True (o_actual `matches`) o_expected')
              then ""
              else showExpectedActual "stdout"    (fromJust o_expected') o_actual
            ,if (maybe True (e_actual `matches`) e_expected')
              then ""
              else showExpectedActual "stderr"    (fromJust e_expected') e_actual
            ,if (maybe True (show x_actual `matches`) x_expected')
              then ""
              else showExpectedActual "exit code" (fromJust x_expected') (show x_actual)
            ]
       where addnewline "" = ""
             addnewline s  = "\n"++s

-- | Run a shell command line, passing it standard input if provided,
-- and return the standard output, standard error output and exit code.
runCommandWithInput :: String -> Maybe String -> IO (String, String, Int)
runCommandWithInput cmd input = do
  -- this has to be done carefully.
  (ih,oh,eh,ph) <- runInteractiveCommand cmd
  when (isJust input) $ forkIO (hPutStr ih $ fromJust input) >> return ()
  o <- newEmptyMVar
  e <- newEmptyMVar
  forkIO $ oh `hGetContentsStrictlyAnd` putMVar o
  forkIO $ eh `hGetContentsStrictlyAnd` putMVar e
  x_actual <- waitForProcess ph >>= return.fromExitCode
  o_actual <- takeMVar o
  e_actual <- takeMVar e
  return (o_actual, e_actual, x_actual)

hGetContentsStrictlyAnd :: Handle -> (String -> IO b) -> IO b
hGetContentsStrictlyAnd h f = hGetContents h >>= \c -> length c `seq` f c

matches :: String -> Matcher -> Bool
matches s (PositiveRegex r) = s `containsRegex` r
matches s (NegativeRegex r) = not $ s `containsRegex` r
matches s (Numeric p)       = s == p
matches s (Lines p)         = s == p

showExpectedActual :: String -> Matcher -> String -> String
showExpectedActual field e a =
    printf "**Expected %s:%s**Got %s:\n%s" field (showMatcher e) field a

showMatcher :: Matcher -> String
showMatcher (PositiveRegex r) = " /"++r++"/\n"
showMatcher (NegativeRegex r) = " !/"++r++"/\n"
showMatcher (Numeric s)       = "\n"++s++"\n"
showMatcher (Lines s)         = "\n"++s


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
