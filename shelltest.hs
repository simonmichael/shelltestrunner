{-# LANGUAGE DeriveDataTypeable, CPP, RankNTypes #-}
{- |

shelltest - for testing command-line programs. See shelltestrunner.cabal.
(c) Simon Michael 2009-2014, released under GNU GPLv3 or later.

-}

module Main
where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Data.Algorithm.Diff
import Data.Monoid (mempty)
import Data.Version (showVersion)
import Options.Applicative hiding (ParseError, command)
import System.Directory (doesDirectoryExist)
import System.Environment (getArgs)
import System.FilePath (takeDirectory)
import System.FilePath.Find (findWithHandler, (==?), always)
import qualified System.FilePath.Find as Find (extension)
import System.IO (Handle, hGetContents, hPutStr)
import System.Process (StdStream (CreatePipe), shell, createProcess, CreateProcess (..), waitForProcess, ProcessHandle)
import Test.Framework (defaultMainWithArgs)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit
import Text.Parsec hiding (many, option)

import Paths_shelltestrunner (version)
import Import
import Utils
import Types
import Parse


progname, progversion :: String
progname = "shelltest"
progversion = progname ++ " " ++ showVersion version
helpheader = progversion
helpfooter = unlines [
  ]
formathelp = unlines [
   "Test format:"
  ,""
  ,"# optional comments"
  ,"one-line shell command (required; indent to disable --with substitution)"
  ,"<<<"
  ,"0 or more lines of stdin input"
  ,">>>"
  ,"0 or more lines of expected stdout output (or /regexp/ on the previous line)"
  ,">>>2"
  ,"0 or more lines of expected stderr output (or /regexp/ on the previous line)"
  ,">>>= 0 (or other expected numeric exit status, or /regexp/) (required)"
  ,""
  ]

data Args = Args {
     all_        :: Bool
    ,color       :: Bool
    ,diff        :: Bool
    ,precise     :: Bool
    ,hide_successes :: Bool
    ,include     :: [String]
    ,exclude     :: [String]
    ,extension   :: String
    ,with        :: String
    ,execdir     :: Bool
    ,timeout     :: Int
    ,threads     :: Int
    ,debug       :: Bool
    ,debug_parse :: Bool
    ,help_format :: Bool
    ,testpaths   :: [FilePath]
    } deriving (Show)

argsparser :: Parser Args
argsparser = Args
  <$> switch          (long "all"            <> short 'a'                                           <> h "Show all failure output, even if large")
  <*> switch          (long "color"          <> short 'c'                                           <> h "Show colored output if your terminal supports it")
  <*> switch          (long "diff"           <> short 'd'                                           <> h "Show failures in diff format")
  <*> switch          (long "precise"        <> short 'p'                                           <> h "Show failure output precisely (good for whitespace)")
  <*> switch          (long "hide-successes" <> short 'h'                                           <> h "Report only failed tests")
  <*> many (strOption (long "include"        <> short 'i' <> m "PAT"               <> n "include"   <> h "Include tests whose name contains this glob pattern"))
  <*> many (strOption (long "exclude"        <> short 'x' <> m "STR"               <> n "exclude"   <> h "Exclude test files whose path contains STR"))
  <*> strOption       (long "extension"                   <> m "EXT"  <> v ".test" <> n "extension" <> h "Filename suffix of test files (default: .test)")
  <*> strOption       (long "with"           <> short 'w' <> m "EXE"  <> v ""      <> n "with"      <> h "Replace the first word of (unindented) test commands")
  <*> switch          (long "execdir"                                                               <> h "Run tests from within the test file's directory")
  <*> option auto     (long "timeout"        <> short 'o' <> m "SECS" <> v 0       <> n "timeout"   <> h "Number of seconds a test may run (default: no limit)")
  <*> option auto     (long "threads"        <> short 'j' <> m "N"    <> v 1       <> n "threads"   <> h "Number of threads for running tests (default: 1)")
  <*> switch          (long "debug"                                                                 <> h "Show debug info, for troubleshooting")
  <*> switch          (long "debug-parse"                                                           <> h "Show test file parsing info and stop")
  <*> switch          (long "help-format"                                                           <> h "Display test format help")
  <*> some (strArgument (metavar "\nFILES/DIRS..."))
  where
    -- (l, s, m, v, n, h) = (long, short, metavar, value, needsarg, help)
    (m, v, n, h) = (metavar, value, needsarg, help)
    needsarg s = noArgError $ ErrorMsg ("--" ++ s ++ " needs an argument")
-- XXX issues
-- need a precise error on missing arguments (FILES/DIRS displayed with [] if we do it ourselves)
-- --help is not listed in short usage message

argsinfo :: ParserInfo Args
argsinfo = info
  (helper <*> argsparser)
  (header helpheader <> footer helpfooter)

-- | Additional argument checking.
extraArgsChecks :: Args -> IO Args
extraArgsChecks args = do
  -- give a better args-required error message than optparse-applicative
  -- when (null $ testpaths args) $
  --   parserError (printf "Please specify at least one test file or directory, eg: %s tests" progname)
  return args

-- parserError :: String -> IO ()
-- parserError msg = do
--   let f = parserFailure parserprefs argsinfo (ErrorMsg msg) mempty
--       (s, code) = renderFailure f progname
--   putStrLn s >> exitWith code

parserprefs = ParserPrefs
  { prefMultiSuffix=""        -- ^ metavar suffix for multiple options
  , prefDisambiguate=False    -- ^ automatically disambiguate abbreviations (default: False)
  , prefShowHelpOnError=False -- ^ always show help text on parse errors (default: False)
  , prefBacktrack=True        -- ^ backtrack to parent parser when a subcommand fails (default: True)
  , prefColumns=80            -- ^ number of columns in the terminal, used to format the help page (default: 80)
  }

main :: IO ()
main = do
  -- parse args
  rawargs <- getArgs
  args <- handleParseResult (execParserPure parserprefs argsinfo rawargs) >>= extraArgsChecks
  when (debug args) $ printf "%s\n" progversion >> printf "args: %s\n" (ppShow args)
  let tfargs =    if color args then [] else ["--plain"]
               ++ if hide_successes args then ["--hide-successes"] else []
               ++ ["--select-tests="++s | s <- include args]
               ++ if timeout args > 0 then ["--timeout=" ++ show (timeout args)] else []
               ++ if threads args > 1 then ["--threads=" ++ show (threads args)] else []

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
  when (excluded > 0) $ printf "Excluding %d test files\n" excluded

  -- parse test files
  when (debug args) $ printf "processing %d test files: %s\n" (length testfiles) (intercalate ", " testfiles)
  testparseresults <- mapM (parseShellTestFile (debug args || debug_parse args)) testfiles

  -- run tests
  when (debug args) $ printf "running tests:\n"
  unless (debug_parse args) $
    flip defaultMainWithArgs tfargs $ concatMap (hUnitTestToTests . testFileParseToHUnitTest args) testparseresults


-- running tests

testFileParseToHUnitTest :: Args -> Either ParseError [ShellTest] -> Test.HUnit.Test
testFileParseToHUnitTest args (Right ts) = TestList $ map (shellTestToHUnitTest args) ts
testFileParseToHUnitTest _ (Left e) = ("parse error in " ++ (sourceName $ errorPos e)) ~: assertFailure $ show e

shellTestToHUnitTest :: Args -> ShellTest -> Test.HUnit.Test
shellTestToHUnitTest args ShellTest{testname=n,command=c,stdin=i,stdoutExpected=o_expected,
                                    stderrExpected=e_expected,exitCodeExpected=x_expected} = 
 n ~: do
  let e = with args
      cmd = case (e,c) of (_:_, ReplaceableCommand s) -> e ++ " " ++ dropWhile (/=' ') s
                          (_, ReplaceableCommand s)   -> s
                          (_, FixedCommand s)         -> s
      dir = if execdir args then Just $ takeDirectory n else Nothing
      trim' = if all_ args then id else trim
  when (debug args) $ do
    printf "actual command was: %s\n" (show cmd)
  (o_actual, e_actual, x_actual) <- runCommandWithInput dir cmd i
  when (debug args) $ do
    printf "actual stdout was : %s\n" (show $ trim' o_actual)
    printf "actual stderr was : %s\n" (show $ trim' e_actual)
    printf "actual exit was   : %s\n" (trim' $ show x_actual)
  let outputMatch = maybe True (o_actual `matches`) o_expected
  let errorMatch = maybe True (e_actual `matches`) e_expected
  let exitCodeMatch = show x_actual `matches` x_expected
  let matches = [outputMatch, errorMatch, exitCodeMatch]
  if (x_actual == 127) -- catch bad executable - should work on posix systems at least
   then ioError $ userError $ unwords $ filter (not . null) [e_actual, printf "Command: '%s' Exit code: %i" cmd x_actual] -- XXX still a test failure; should be an error
   else assertString $ addnewline $ intercalate "\n" $ filter (not . null) [
             if any not matches
               then printf "Command:\n%s\n" cmd
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
       where addnewline "" = ""
             addnewline s  = "\n"++s

-- | Run a shell command line, passing it standard input if provided,
-- and return the standard output, standard error output and exit code.
-- Note on unix, at least with ghc 6.12, command (and filepath) are assumed to be utf8-encoded.
runCommandWithInput :: Maybe FilePath -> String -> Maybe String -> IO (String, String, Int)
runCommandWithInput wd cmd input = do
  -- this has to be done carefully
  (ih,oh,eh,ph) <- runInteractiveCommandInDir wd cmd 
  when (isJust input) $ forkIO (hPutStr ih $ fromJust input) >> return ()
  o <- newEmptyMVar
  e <- newEmptyMVar
  forkIO $ oh `hGetContentsStrictlyAnd` putMVar o
  forkIO $ eh `hGetContentsStrictlyAnd` putMVar e
  x_actual <- waitForProcess ph >>= return.fromExitCode
  o_actual <- takeMVar o
  e_actual <- takeMVar e
  return (o_actual, e_actual, x_actual)

runInteractiveCommandInDir :: Maybe FilePath -> String ->  IO (Handle, Handle, Handle, ProcessHandle)
runInteractiveCommandInDir wd cmd = do
   (mb_in, mb_out, mb_err, p) <- 
      createProcess $ 
         (shell cmd) { cwd = wd
                     , std_in  = CreatePipe
                     , std_out = CreatePipe
                     , std_err = CreatePipe }
   return (fromJust mb_in, fromJust mb_out, fromJust mb_err, p)

hGetContentsStrictlyAnd :: Handle -> (String -> IO b) -> IO b
hGetContentsStrictlyAnd h f = hGetContents h >>= \s -> length s `seq` f s

matches :: String -> Matcher -> Bool
matches s (PositiveRegex r)   = s `containsRegex` r
matches s (NegativeRegex r)   = not $ s `containsRegex` r
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

