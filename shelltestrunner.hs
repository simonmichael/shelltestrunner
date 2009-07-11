#!/usr/bin/env runhaskell
{-

shelltestrunner - a tool for testing command-line programs.

Run a given program through "shell" tests specifed by one or more test
files, each of which specifies: command-line arguments, input,
expected output, expected stderr output, and expected exit code.  This
was extracted from the hledger project, and inspired by the tests in
John Wiegley's ledger project.

This uses test-framework's test runner.  Output order is currently a
bit mixed up. Any command-line options are passed through to the
test-framework runner, but they must not contain spaces, so use eg
-tpattern not -t pattern. You may be able to get a big speedup by
running tests in parallel: try -j8.

Usage: 

$ ghc --make -threaded ./shelltestrunner.hs
$ ./shelltestrunner [testrunneropts] executable testfile1 [testfile2 ...]

Test file format:

@
-opt1 -opt2 arg1 arg2
<<<
0 or more lines of input
>>>
0 or more lines of expected output
>>>2
0 or more lines of expected error output
<<<expected numeric exit code>>>
@

Lines whose first non-whitespace character is ; are ignored, mostly.
The first line is the command line, to be appended to the executable name.
All remaining fields line are optional; when omitted they are assumed
to be "", "", "", and 0 respectively.

Issues:


(c) Simon Michael 2009, released under GNU GPLv3

-}

module Main
where
import Control.Monad (liftM,when)
import Data.List (partition)
import Data.Maybe (fromMaybe)
import qualified Test.HUnit (Test)
import System.Environment (getArgs,withArgs)
import System.Exit (ExitCode(..))
import System.IO (hGetContents, hPutStr, stderr)
import System.Process (runInteractiveCommand, waitForProcess)
import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit hiding (Test)
import Text.ParserCombinators.Parsec
import Text.Printf (printf)
-- import Debug.Trace
-- strace :: Show a => a -> a
-- strace a = trace (show a) a

data ShellTest = ShellTest {
     filename         :: String
    ,command          :: String
    ,stdin            :: Maybe String
    ,stdoutExpected   :: Maybe String
    ,stderrExpected   :: Maybe String
    ,exitCodeExpected :: Maybe ExitCode
    } deriving (Show)

main :: IO ()
main = do
  args <- getArgs
  let (opts,args') = partition ((=="-").take 1) args
      args'' = if null args' && ("--help" `elem` opts) then [""] else args' -- temp
      (exe:files) = args''
  shelltests <- mapM parseShellTest files
  withArgs opts $ defaultMain $ concatMap (hUnitTestToTests.shellTestToHUnitTest exe) shelltests

shellTestToHUnitTest :: FilePath -> ShellTest -> Test.HUnit.Test
shellTestToHUnitTest exe t = filename t ~: do {r <- runShellTest exe t; assertBool "" r}

parseShellTest :: FilePath -> IO ShellTest
parseShellTest = liftM (either (error.show) id) . parseFromFile shelltest

shelltest :: Parser ShellTest
shelltest = do
  st <- getParserState
  let f = sourceName $ statePos st
  c <- commandline
  i <- optionMaybe input
  o <- optionMaybe expectedoutput
  e <- optionMaybe expectederror
  x <- optionMaybe expectedexitcode
  return ShellTest{filename=f,command=c,stdin=i,stdoutExpected=o,stderrExpected=e,exitCodeExpected=x}

commandline,input,expectedoutput,expectederror,delimiter,line :: Parser String
commandline = line
input = string "<<<\n" >> (liftM unlines) (line `manyTill` (lookAhead delimiter))
expectedoutput = try $ string ">>>" >> optional (char '1') >> char '\n' >> (liftM unlines) (line `manyTill` (lookAhead delimiter))
expectederror = string ">>>2" >> (liftM $ unlines.tail) (line `manyTill` (lookAhead delimiter)) -- why tail ?
delimiter = choice [try $ string "<<<", try $ string ">>>", (eof >> return "")]
line =  do
  l <- anyChar `manyTill` newline
  if take 1 (strip l) == ";" then line else return l

expectedexitcode :: Parser ExitCode
expectedexitcode = do
  string "<<<"
  c <- liftM (toExitCode.read) line
  string ">>>"
  return c

runShellTest :: FilePath -> ShellTest -> IO Bool
runShellTest exe ShellTest{
    filename=_,command=c,stdin=i,stdoutExpected=o,stderrExpected=e,exitCodeExpected=x} = do
  let cmd = unwords [exe,c]
      (i',o',e',x') = (fromMaybe "" i, fromMaybe "" o, fromMaybe "" e, fromMaybe ExitSuccess x)
  -- printf "%s .. " f; hFlush stdout
  (ih,oh,eh,ph) <- runInteractiveCommand cmd
  hPutStr ih i'
  out <- hGetContents oh
  err <- hGetContents eh
  exit <- waitForProcess ph
  let (outputok, errorok, exitok) = (out==o', err==e', exit==x')
  if outputok && errorok && exitok 
   then do
     -- putStrLn "ok"
     return True 
   else do
     -- hPutStr stderr $ printf "FAIL\n"
     when (not outputok) $ printExpectedActual "stdout" o' out
     when (not errorok)  $ printExpectedActual "stderr" e' err
     when (not exitok)   $ printExpectedActual "exit code" (show (fromExitCode x')++"\n") (show (fromExitCode exit)++"\n")
     return False

printExpectedActual :: String -> String -> String -> IO ()
printExpectedActual f e a = hPutStr stderr $ printf "**Expected %s:\n%s**Got:\n%s" f e a

toExitCode :: Int -> ExitCode
toExitCode 0 = ExitSuccess
toExitCode n = ExitFailure n

fromExitCode :: ExitCode -> Int
fromExitCode ExitSuccess     = 0
fromExitCode (ExitFailure n) = n

strip,lstrip,rstrip,dropws :: String -> String
strip = lstrip . rstrip
lstrip = dropws
rstrip = reverse . dropws . reverse
dropws = dropWhile (`elem` " \t")
