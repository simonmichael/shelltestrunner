{-# LANGUAGE DeriveDataTypeable, CPP, DoAndIfThenElse #-}
{- |

shelltest - for testing command-line programs. See shelltestrunner.cabal.
(c) Simon Michael & contributors 2009-2023, released under GNU GPLv3 or later.

-}

module Main
where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (void)
import Data.Algorithm.Diff
import System.Console.CmdArgs
import System.Directory (doesDirectoryExist)
import System.FilePath (takeDirectory)
import System.FilePath.Find (findWithHandler, (==?), always)
import qualified System.FilePath.Find as Find (extension)
import System.IO (Handle, hGetContents, hPutStr)
import System.Process (StdStream (CreatePipe), shell, proc, createProcess, CreateProcess (..), waitForProcess, ProcessHandle)
import Test.Framework (defaultMainWithArgs)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit
import Text.Parsec
import Test.Hspec.Core.Runner (defaultConfig, runSpec, Config (..))
import Test.Hspec.Contrib.HUnit (fromHUnitTest)

import Import
import Utils
import Types
import Parse
import Print
import Preprocessor


progname, progversion :: String
progname = "shelltest"
progversion = progname ++ " " ++ "1.10"
proghelpsuffix :: [String]
proghelpsuffix = [
   "shelltest file formats, tried in this order:"
  ,""
  ,"Description                                            Delimiters, in order"    
  ,"------------------------------------------------------ ------------------------"
  ,"v2 input then multiple tests; some delimiters optional <<<    $$$ >>> >>>2 >>>="
  ,"v3 same as v2, but with short delimiters               <      $   >   >2   >="  
  ,"v1 command first; exit status is required              (none) <<< >>> >>>2 >>>="
  ,""
  ]

data Args = Args {
     list        :: Bool
    ,include     :: [String]
    ,exclude     :: [String]
    ,all_        :: Bool
    ,color       :: Bool
    ,diff        :: Bool
    ,precise     :: Bool
    ,hide_successes :: Bool
    ,fail_fast   :: Bool
    ,xmlout      :: String
    ,defmacro    :: [String]
    ,execdir     :: Bool
    ,extension   :: String
    ,with        :: String
    ,timeout     :: Int
    ,threads     :: Int
    ,shell_cmd   :: Maybe FilePath
    ,debug       :: Bool
    ,debug_parse :: Bool
    ,testpaths   :: [FilePath]
    ,print_      :: Maybe String
    ,hspec_      :: Bool
    } deriving (Show, Data, Typeable)

argdefs = Args {
     list        = def     &= help "List the names of all tests found"
    ,include     = def     &= name "i" &= typ "PAT" &= help "Include tests whose name contains this glob pattern (eg: -i1 -i{4,5,6})"
    ,exclude     = def     &= name "x" &= typ "STR" &= help "Exclude test files whose path contains STR"
    ,all_        = def     &= help "Show all output without truncating, even if large"
    ,color       = def     &= help "Show colored output if your terminal supports it"
    ,diff        = def     &= name "d" &= help "Show differences between expected/actual output"
    ,precise     = def     &= name "p" &= help "Show expected/actual output precisely, with quoting"
    ,hide_successes = def  &= explicit &= name "hide-successes" &= help "Show only test failures"
    ,fail_fast   = def     &= explicit &= name "fail-fast" &= help "Only hspec: stop tests on first failure"
    ,xmlout      = def     &= typ "FILE" &= help "Save test results to FILE in XML format."
    ,defmacro    = def  &= name "D" &= typ "D=DEF" &= help "Define a macro D to be replaced by DEF while parsing test files."
    ,execdir     = def     &= help "Run tests from within each test file's directory"
    ,extension   = ".test" &= typ "EXT" &= help "File suffix of test files (default: .test)"
    ,with        = def     &= typ "EXE" &= help "Replace the first word of test commands with EXE (unindented commands only)"
    ,timeout     = def     &= name "o" &= typ "SECS" &= help "Number of seconds a test may run (default: no limit)"
    ,threads     = def     &= name "j" &= typ "N" &= help "Number of threads for running tests (default: 1)"
    ,shell_cmd   = def     &= explicit &= name "shell" &= typ "EXE" &= help "The shell program to use (must accept -c CMD; default: /bin/sh on POSIX, cmd.exe on Windows)"
    ,debug       = def     &= help "Show debug info while running"
    ,debug_parse = def     &= help "Show test file parsing results and stop"
    ,testpaths   = def     &= args &= typ "TESTFILES|TESTDIRS"
    ,print_      = def     &= typ "FORMAT" &= opt "v3" &= groupname "Print test file" &= help "Print test files in specified format (default: v3)."
    ,hspec_      = def     &= name"hspec" &= help "Use hspec to run tests."
    }
    &= helpArg [explicit, name "help", name "h"]
    &= program progname
    &= summary progversion
    &= details proghelpsuffix

main :: IO ()
main = do
  -- parse args
  args' <- cmdArgs argdefs >>= checkArgs
  -- some of the cmdargs-parsed "arguments" may be test-framework options following --,
  -- separate those out
  let (tfopts', realargs) = partition ("-" `isPrefixOf`) $ testpaths args'
      args = args'{testpaths=realargs}
      tfopts = tfopts'
               ++ (if list args then ["--list"] else [])
               ++ (if color args then [] else ["--plain"])
               ++ (if hide_successes args then ["--hide-successes"] else [])
               ++ (["--select-tests="++s | s <- include args])
               ++ (if timeout args > 0 then ["--timeout=" ++ show (timeout args)] else [])
               ++ (if threads args > 0 then ["--threads=" ++ show (threads args)] else [])
               ++ (if not (xmlout args == []) then ["--jxml=" ++ (xmlout args)] else [])
      hspecconf = defaultConfig
                  {configConcurrentJobs = Just (threads args)
                  ,configFailFast = fail_fast args
                  -- TODO add other options; compare shelltest -h and http://hspec.github.io/options.html
                  -- https://hackage.haskell.org/package/hspec-core-2.7.2/docs/Test-Hspec-Core-Runner.html#g:2
                  }


  when (debug args) $ printf "%s\n" progversion >> printf "args: %s\n" (ppShow args)

  -- gather test files
  testfiles' <- nub . concat <$> mapM
                                 (\p -> do
                                     isdir <- doesDirectoryExist p
                                     if isdir
                                       then findWithHandler (\_ e->fail (show e)) always (Find.extension ==? extension args) p
                                       else return [p])
                                 (testpaths args)
  let testfiles = filter (not . \p -> any (`isInfixOf` p) (exclude args)) testfiles'
      excluded = length testfiles' - length testfiles
      macros = (getMacros (defmacro args))
      preprocessor = createMacroPreProcessor macros
  when (excluded > 0) $ printf "Excluding %d test files\n" excluded

  -- parse test files
  when (debug args) $ printf "processing %d test files: %s\n" (length testfiles) (intercalate ", " testfiles)
  shelltests <- mapM (parseShellTestFile (debug args || debug_parse args) preprocessor) testfiles

  -- exit after --debug-parse output
  if debug_parse args then exitSuccess
  -- or print tests
  else if isJust $ print_ args then mapM_ (printShellTestsWithResults args) shelltests
  -- or run tests
  else do
    when (debug args) $ printf "running tests:\n"
    if hspec_ args
      then runWithHspec         args hspecconf shelltests
      else runWithTestFramework args tfopts    shelltests

runWithTestFramework :: Args -> [String] -> [Either ParseError [ShellTest]] -> IO ()
runWithTestFramework args tfopts shelltests = defaultMainWithArgs hunittests tfopts
  where hunittests = concatMap (hUnitTestToTests . testFileParseToHUnitTest args) shelltests

runWithHspec :: Args -> Config -> [Either ParseError [ShellTest]] -> IO ()
runWithHspec args hsconf shelltests = void $ runSpec spec hsconf
  where
    spec = fromHUnitTest $ TestLabel "All shelltests" $ TestList hunittests
    hunittests = map (testFileParseToHUnitTest args) shelltests

printShellTestsWithResults :: Args -> Either ParseError [ShellTest] -> IO ()
printShellTestsWithResults args (Right ts) = mapM_ (prepareShellTest args) ts
printShellTestsWithResults _ (Left e) = putStrLn $ "*** parse error in " ++ (sourceName $ errorPos e)

-- | Additional argument checking.
checkArgs :: Args -> IO Args
checkArgs args = do
  when (null $ testpaths args) $
       warn $ printf "Please specify at least one test file or directory, eg: %s tests" progname
  return args

-- running tests

testFileParseToHUnitTest :: Args -> Either ParseError [ShellTest] -> Test.HUnit.Test
testFileParseToHUnitTest args (Right ts) = TestList $ map (\t -> testname t ~: prepareShellTest args t) ts
testFileParseToHUnitTest _ (Left e) = ("parse error in " ++ (sourceName $ errorPos e)) ~: (assertFailure :: (String -> IO ())) $ show e

-- | Convert this test to an IO action that either runs the test or prints it
-- to stdout, depending on the arguments.
prepareShellTest :: Args -> ShellTest -> IO ()
prepareShellTest args st@ShellTest{testname=n,command=c,stdin=i,stdoutExpected=o_expected,
                       stderrExpected=e_expected,exitCodeExpected=x_expected,lineNumber=ln} =
 do
  let e = with args
      cmd = case (e,c) of (_:_, ReplaceableCommand s) -> e ++ " " ++ dropWhile (/=' ') s
                          (_, ReplaceableCommand s)   -> s
                          (_, FixedCommand s)         -> s
      dir = if execdir args then Just $ takeDirectory n else Nothing
      trim' = if all_ args then id else trim
  when (debug args) $ do
    printf "actual command was: %s\n" (show cmd)
  (o_actual, e_actual, x_actual) <- runCommandWithInput (shell_cmd args) dir cmd i
  when (debug args) $ do
    printf "actual stdout was : %s\n" (show $ trim' o_actual)
    printf "actual stderr was : %s\n" (show $ trim' e_actual)
    printf "actual exit was   : %s\n" (trim' $ show x_actual)
  let outputMatch = maybe True (o_actual `matches`) o_expected
  let errorMatch = maybe True (e_actual `matches`) e_expected
  let exitCodeMatch = show x_actual `matches` x_expected
  case print_ args of
    Just format -> printShellTest format st
    Nothing -> if (x_actual == 127) -- catch bad executable - should work on posix systems at least
           then ioError $ userError $ unwords $ filter (not . null) [e_actual, printf "Command: '%s' Exit code: %i" cmd x_actual] -- XXX still a test failure; should be an error
           else assertString $ concat $ filter (not . null) [
             if any not [outputMatch, errorMatch, exitCodeMatch]
               then printf "Command (at line %s):\n%s\n" (show ln) cmd
               else ""
            ,if outputMatch
              then ""
              else showExpectedActual args "stdout"    (fromJust o_expected) o_actual
            ,if errorMatch
              then ""
              else showExpectedActual args "stderr"    (fromJust e_expected) e_actual
            ,if exitCodeMatch
              then ""
              else showExpectedActual args{diff=False} "exit code" x_expected (show x_actual)
            ]

-- | Run a shell command line, passing it standard input if provided,
-- and return the standard output, standard error output and exit code.
-- Note on unix, at least with ghc 6.12, command (and filepath) are assumed to be utf8-encoded.
runCommandWithInput :: Maybe FilePath -> Maybe FilePath -> String -> Maybe String -> IO (String, String, Int)
runCommandWithInput sh wd cmd input = do
  -- this has to be done carefully
  (ih,oh,eh,ph) <- runInteractiveCommandInDir sh wd cmd
  when (isJust input) $ forkIO (hPutStr ih $ fromJust input) >> return ()
  o <- newEmptyMVar
  e <- newEmptyMVar
  forkIO $ oh `hGetContentsStrictlyAnd` putMVar o
  forkIO $ eh `hGetContentsStrictlyAnd` putMVar e
  x_actual <- waitForProcess ph >>= return.fromExitCode
  o_actual <- takeMVar o
  e_actual <- takeMVar e
  return (o_actual, e_actual, x_actual)

runInteractiveCommandInDir :: Maybe FilePath -> Maybe FilePath -> String ->  IO (Handle, Handle, Handle, ProcessHandle)
runInteractiveCommandInDir sh wd cmd = do
   (mb_in, mb_out, mb_err, p) <-
      createProcess $
         run { cwd = wd
             , std_in  = CreatePipe
             , std_out = CreatePipe
             , std_err = CreatePipe }
   -- these should all be Just since we used CreatePipe
   return (fromJust mb_in, fromJust mb_out, fromJust mb_err, p)
   where
      run = maybe (shell cmd) (\shcmd -> proc shcmd ["-c", cmd]) sh

hGetContentsStrictlyAnd :: Handle -> (String -> IO b) -> IO b
hGetContentsStrictlyAnd h f = hGetContents h >>= \s -> length s `seq` f s

matches :: String -> Matcher -> Bool
matches s (PositiveRegex (MR r))   = s `containsRegex` r
matches s (NegativeRegex (MR r))   = not $ s `containsRegex` r
matches s (Numeric p)         = s == p
matches s (NegativeNumeric p) = not $ s == p
matches s (Lines _ p)         = s == p

showExpectedActual :: Args -> String -> Matcher -> String -> String
showExpectedActual args@Args{diff=True} _ (Lines ln e) a =
    printf "--- Expected\n+++ Got\n" ++ showDiff args(1,ln) (getDiff (lines a) (lines e))
showExpectedActual Args{all_=all_,precise=precise} field e a =
    printf "Expected %s: %s\nGot %s:      %s" field (show' $ showm e) field (show' $ trim' a)
    where
      show' = if precise then show else ("\n"++)
      showm = if all_ then showMatcher else showMatcherTrimmed
      trim' = if all_ then id else trim

showDiff :: Args -> (Int,Int) -> [(Diff String)] -> String
showDiff _ _ []                   = ""
showDiff args@Args{all_=all_,precise=precise} (l,r) ((First ln) : ds) =
    printf "+%4d " l ++ ln' ++ "\n" ++ showDiff args (l+1,r) ds
    where
      ln' = trim' $ show' ln
      trim' = if all_ then id else trim
      show' = if precise then show else id
showDiff args@Args{all_=all_,precise=precise} (l,r) ((Second ln) : ds) =
    printf "-%4d " r ++ ln' ++ "\n" ++ showDiff args (l,r+1) ds
    where
      ln' = trim' $ show' ln
      trim' = if all_ then id else trim
      show' = if precise then show else id
showDiff args (l,r) ((Both _ _) : ds) = showDiff args (l+1,r+1) ds
