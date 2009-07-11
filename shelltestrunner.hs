#!/usr/bin/env runhaskell
{-

shelltestrunner - a handy tool for testing command-line programs.

See shelltestrunner.cabal.

(c) Simon Michael 2009, released under GNU GPLv3

-}

module Main
where
import Control.Monad (liftM,when)
import Data.List (partition)
import Data.Maybe (fromMaybe,fromJust)
import qualified Test.HUnit (Test)
import System.Console.ParseArgs
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

data ArgId = HelpFlag
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
 ,Arg {argIndex = ExecutableArg,
       argName  = Nothing,
       argAbbr  = Nothing,
       argData  = argDataRequired "executable" ArgtypeString,
       argDesc  = "executable program or shell command to test, should accept stdin"
      }
 ]

main :: IO ()
main = do
  args <- parseArgsIO ArgsInterspersed argspec
  when (gotArg args HelpFlag) $ usageError args "" -- never gets this far
  let (unprocessedopts, testfiles) = partition ((=="-").take 1) $ argsRest args
      exe = fromJust $ getArgString args ExecutableArg
  shelltests <- mapM parseShellTest testfiles
  withArgs unprocessedopts $ defaultMain $ concatMap (hUnitTestToTests.shellTestToHUnitTest exe) shelltests

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

line,commandline,delimiter,input,expectedoutput,expectederror :: Parser String

line =  do
  l <- anyChar `manyTill` newline
  if take 1 (strip l) == ";" then line else return l

commandline = line

delimiter = choice [try $ string "<<<", try $ string ">>>", (eof >> return "")]

input = string "<<<\n" >> (liftM unlines) (line `manyTill` (lookAhead delimiter))

expectedoutput = try $ string ">>>" >> optional (char '1') >> newline >> (liftM unlines) (line `manyTill` (lookAhead delimiter))

expectederror = try $ string ">>>2\n" >> (liftM unlines) (line `manyTill` (lookAhead delimiter))

expectedexitcode :: Parser ExitCode
expectedexitcode = string ">>>=" >> (liftM (toExitCode.read.unlines) (line `manyTill` eof))

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
