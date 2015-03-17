{-# LANGUAGE DeriveDataTypeable, CPP #-}
{- |

shelltest - for testing command-line programs. See shelltestrunner.cabal.
(c) Simon Michael 2009-2014, released under GNU GPLv3 or later.

-}

module Main
where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Data.Algorithm.Diff
import Data.Version (showVersion)
import System.Console.CmdArgs
import System.Directory (doesDirectoryExist)
import System.FilePath (takeDirectory)
import System.FilePath.Find (findWithHandler, (==?), always)
import qualified System.FilePath.Find as Find (extension)
import System.IO (Handle, hGetContents, hPutStr)
import System.Process (StdStream (CreatePipe), shell, createProcess, CreateProcess (..), waitForProcess, ProcessHandle)
import Test.Framework (defaultMainWithArgs)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit
import Text.Parsec

import Paths_shelltestrunner (version)
import Import
import Utils
import Types
import Parse


progname, progversion :: String
progname = "shelltest"
progversion = progname ++ " " ++ showVersion version
proghelpsuffix :: [String]
proghelpsuffix = [
   -- keep this bit synced with options width
  ]
formathelp :: String
formathelp = unlines [
   "shelltestrunner tries these formats, in order:"
  ,""
  ,"--------------------------------------"
  ,"Test format 2"
  ,""
  ,"# COMMENTS OR BLANK LINES"
  ,"<<<"
  ,"INPUT"
  ,"$$$ COMMAND LINE"
  ,">>>"
  ,"EXPECTED OUTPUT (OR >>> /REGEX/)"
  ,">>>2"
  ,"EXPECTED STDERR (OR >>>2 /REGEX/)"
  ,">>>= EXPECTED EXIT STATUS (OR >>>= /REGEX/)"
  ,"# COMMENTS OR BLANK LINES"
  ,"ADDITIONAL TESTS FOR THIS INPUT"
  ,"ADDITIONAL TEST GROUPS WITH DIFFERENT INPUT"
  ,""
  ,"All parts are optional except the command line."
  ,"If not specified, stdout and stderr are expected to be empty"
  ,"and exit status is expected to be zero."
  ,""
  ,"The <<< delimiter is optional for the first input in a file."
  ,"Without it, input begins at the first non-blank/comment line."
  ,"Input ends at the $$$ delimiter. You can't put a comment before the first $$$."
  ,""
  ,"The >>> delimiter is optional except when matching via regex."
  ,"Expected output/stderr extends to the next >>>2 or >>>= if present,"
  ,"or to the last non-blank/comment line before the next <<< or $$$ or file end."
  ,">>>= with nothing after it ignores the exit status."
  ,""
  ,"Two spaces between $$$ and the command protects it from -w/--with."
  ,"!/REGEX/ negates a regular expression match."
  ,""
  ,"--------------------------------------"
  ,"Test format 2b"
  ,""
  ,"Same as above, but with short delimiters: < $ > >2 >="
  ,""
  ,"--------------------------------------"
  ,"Test format 1 (deprecated)"
  ,""
  ,"# COMMENTS OR BLANK LINES"
  ,"COMMAND LINE"
  ,"<<<"
  ,"INPUT"
  ,">>>"
  ,"EXPECTED OUTPUT (OR >>> /REGEXP/)"
  ,">>>2"
  ,"EXPECTED STDERR (OR >>>2 /REGEXP/)"
  ,">>>= EXPECTED EXIT STATUS (OR >>>= /REGEXP/)"
  ,""
  ,"All parts are optional except the command line and final >>>= line."
  ,"When not specified, stdout/stderr are ignored."
  ,"A space before the command protects it from -w/--with."
  ,""
  ,"--------------------------------------"
  ]

data Args = Args {
     list        :: Bool
    ,all_        :: Bool
    ,color       :: Bool
    ,diff        :: Bool
    ,precise     :: Bool
    ,hide_successes :: Bool
    ,include     :: [String]
    ,exclude     :: [String]
    ,execdir     :: Bool
    ,extension   :: String
    ,with        :: String
    ,timeout     :: Int
    ,threads     :: Int
    ,debug       :: Bool
    ,debug_parse :: Bool
    ,help_format :: Bool
    ,testpaths   :: [FilePath]
    } deriving (Show, Data, Typeable)

argdefs = Args {
     list        = def     &= help "List all parsed tests and stop"
    ,all_        = def     &= help "Don't truncate output, even if large"
    ,color       = def     &= help "Show colored output if your terminal supports it"
    ,diff        = def     &= name "d" &= help "Show expected output mismatches in diff format"
    ,precise     = def     &= help "Show expected/actual output precisely (eg whitespace)"
    ,hide_successes = def  &= help "Show only test failures"
    ,include     = def     &= name "i" &= typ "PAT" &= help "Include tests whose name contains this glob pattern"
    ,exclude     = def     &= name "x" &= typ "STR" &= help "Exclude test files whose path contains STR"
    ,execdir     = def     &= help "Run tests from within the test file's directory"
    ,extension   = ".test" &= typ "EXT" &= help "File suffix of test files (default: .test)"
    ,with        = def     &= typ "EXECUTABLE" &= help "Replace the first word of (unindented) test commands"
    ,timeout     = def     &= name "o" &= typ "SECS" &= help "Number of seconds a test may run (default: no limit)"
    ,threads     = def     &= name "j" &= typ "N" &= help "Number of threads for running tests (default: 1)"
    ,debug       = def     &= help "Show debug info, for troubleshooting"
    ,debug_parse = def     &= help "Show test file parsing info and stop"
    ,help_format = def     &= explicit &= name "help-format" &= help "Describe the test file format"
    ,testpaths   = def     &= args &= typ "TESTFILES|TESTDIRS"
    }
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
  when (excluded > 0) $ printf "Excluding %d test files\n" excluded

  -- parse test files
  when (debug args) $ printf "processing %d test files: %s\n" (length testfiles) (intercalate ", " testfiles)
  parseresults <- mapM (parseShellTestFile (debug args || debug_parse args)) testfiles

  -- run tests
  when (debug args) $ printf "running tests:\n"
  unless (debug_parse args) $
    defaultMainWithArgs (concatMap (hUnitTestToTests . testFileParseToHUnitTest args) parseresults) tfopts

-- | Additional argument checking.
checkArgs :: Args -> IO Args
checkArgs args = do
  when (help_format args) $ printf formathelp >> exitSuccess
  when (null $ testpaths args) $
       warn $ printf "Please specify at least one test file or directory, eg: %s tests" progname
  return args

-- | Show a message, usage string, and terminate with exit status 1.
warn :: String -> IO ()
warn s = putStrLn s >> exitWith (ExitFailure 1)


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

