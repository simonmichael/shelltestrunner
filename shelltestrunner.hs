#!/usr/bin/env runhaskell
{-

shelltestrunner - a handy tool for testing command-line programs.

See shelltestrunner.cabal.

(c) Simon Michael 2009, released under GNU GPLv3

-}

module Main
where
--import Control.Concurrent (forkIO)
import Control.Monad (liftM,when)
import Data.List (partition)
import Data.Maybe (fromMaybe,fromJust)
import qualified Test.HUnit (Test)
import System.Console.ParseArgs hiding (args)
import System.Environment (withArgs)
import System.Exit (ExitCode(..),exitWith)
import System.IO (hGetContents, hPutStr, stderr)
import System.Process (runInteractiveCommand, waitForProcess)
-- import System.Process (createProcess, shell, CreateProcess(..), StdStream(..))
import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit hiding (Test)
import Text.ParserCombinators.Parsec
import Text.Printf (printf)
import Debug.Trace
strace :: Show a => a -> a
strace a = trace (show a) a

version :: String
version = "0.3" -- sync with .cabal

data ArgId = HelpFlag
           | VersionFlag
           | DebugFlag
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
 ,Arg {argIndex = ExecutableArg,
       argName  = Nothing,
       argAbbr  = Nothing,
       argData  = argDataRequired "executable" ArgtypeString,
       argDesc  = "executable program or shell command to test, should accept stdin"
      }
 ]

data ShellTest = ShellTest {
     testname         :: String
    ,commandargs      :: String
    ,stdin            :: Maybe String
    ,stdoutExpected   :: Maybe String
    ,stderrExpected   :: Maybe String
    ,exitCodeExpected :: Maybe ExitCode
    } deriving (Show)

main :: IO ()
main = do
  args <- parseArgsIO ArgsInterspersed argspec
  -- parseargs issue: exits at first gotArg if there's no exe argument
  when (args `gotArg` VersionFlag) printVersion
  when (args `gotArg` HelpFlag) $ printHelp args
  let (unprocessedopts, testfiles) = partition ((=="-").take 1) $ argsRest args
  when (args `gotArg` DebugFlag) $ do
         putStrLn $ "testing executable: " ++ (show $ fromJust $ getArgString args ExecutableArg)
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

linep,commentlinep,commandargsp,delimiterp,inputp,expectedoutputp,expectederrorp :: Parser String

linep = anyChar `manyTill` newline

commentlinep = char '#' >> anyChar `manyTill` newline

commandargsp = linep

delimiterp = choice [try $ string "<<<", try $ string ">>>", (eof >> return "")]

inputp = string "<<<\n" >> (liftM unlines) (linep `manyTill` (lookAhead delimiterp))

expectedoutputp = try $ string ">>>" >> optional (char '1') >> newline >> (liftM unlines) (linep `manyTill` (lookAhead delimiterp))

expectederrorp = try $ string ">>>2\n" >> (liftM unlines) (linep `manyTill` (lookAhead delimiterp))

expectedexitcodep :: Parser ExitCode
expectedexitcodep = string ">>>=" >> (liftM (toExitCode.read.unlines) (linep `manyTill` eof))

-- running

shellTestToHUnitTest :: Args ArgId -> ShellTest -> Test.HUnit.Test
shellTestToHUnitTest args t = testname t ~: do {r <- runShellTest args t; assertBool "" r}

runShellTest :: Args ArgId -> ShellTest -> IO Bool
runShellTest args ShellTest{testname=_,commandargs=c,stdin=i,stdoutExpected=o,
                            stderrExpected=e,exitCodeExpected=x} = do
  let exe = fromJust $ getArgString args ExecutableArg
      cmd = unwords [exe,c]
      (i',o',e',x') = (fromMaybe "" i, fromMaybe "" o, fromMaybe "" e, fromMaybe ExitSuccess x)
  -- printf "%s .. " f; hFlush stdout

  when (args `gotArg` DebugFlag) $ do
    putStrLn $ "running command: " ++ cmd
  (ih,oh,eh,ph) <- runInteractiveCommand cmd
  -- forkIO $ hPutStr ih i' -- separate thread in case cmd does not read stdin
  -- (Just ih,Just oh,Just eh,ph) <- createProcess $ (shell cmd){std_in=CreatePipe,std_out=CreatePipe,std_err=CreatePipe}
  hPutStr ih i'
  out <- hGetContents oh
  err <- hGetContents eh
  -- on a mac, this hangs if cmd does not read stdin (http://hackage.haskell.org/trac/ghc/ticket/3369)
  -- not always ?
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
printExpectedActual f e a = hPutStr stderr $ printf "**Expected %s:\n%s**Got %s:\n%s" f e f a

-- utils

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
