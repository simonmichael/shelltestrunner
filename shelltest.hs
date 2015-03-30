{-# LANGUAGE DeriveDataTypeable, CPP #-}
{- |

shelltest - a tool for testing command-line programs.

See shelltestrunner.cabal.

(c) Simon Michael 2009-2011, released under GNU GPLv3 or later.

-}

module Main
where
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (liftM,when,unless)
import Data.List
import Data.Maybe (isNothing,isJust,fromJust,catMaybes)
import Data.Version (showVersion)
import qualified Test.HUnit (Test)
import System.Console.CmdArgs
import System.Exit
import System.IO (Handle, hGetContents, hPutStr)
import System.Process (StdStream (CreatePipe), shell, createProcess, CreateProcess (..), waitForProcess, ProcessHandle)
import Test.Framework (defaultMainWithArgs)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit hiding (Test)
import Text.ParserCombinators.Parsec
import Text.Printf (printf)
import Text.Regex.TDFA ((=~))
import Debug.Trace
import System.Directory (doesDirectoryExist)
import System.FilePath (takeDirectory)
import System.FilePath.Find (findWithHandler, (==?), always)
import qualified System.FilePath.Find as Find (extension)
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Data.Algorithm.Diff

import PlatformString (fromPlatformString, toPlatformString)
import Paths_shelltestrunner (version)

strace :: Show a => a -> a
strace a = trace (show a) a

progname, progversion :: String
progname = "shelltest"
progversion = progname ++ " " ++ showVersion version
proghelpsuffix :: [String]
proghelpsuffix = [
   -- keep this bit synced with options width
   "     -- TFOPTIONS       Set extra test-framework options like -j/--threads,"
  ,"                        -t/--select-tests, -o/--timeout, --hide-successes."
  ,"                        Use -- --help for a list. Avoid spaces."
  ,""
  ]
formathelp :: String
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
    ,exclude     :: [String]
    ,execdir     :: Bool
    ,extension   :: String
    ,with        :: String
    ,debug       :: Bool
    ,debug_parse :: Bool
    ,help_format :: Bool
    ,testpaths   :: [FilePath]
    } deriving (Show, Data, Typeable)

argdefs = Args {
     all_        = def     &= help "Show all failure output, even if large"
    ,color       = def     &= help "Show colored output if your terminal supports it"
    ,diff        = def     &= name "d" &= help "Show failures in diff format"
    ,precise     = def     &= help "Show failure output precisely (good for whitespace)"
    ,exclude     = def     &= name "x" &= typ "STR" &= help "Exclude test files whose path contains STR"
    ,execdir     = def     &= help "Run tests from within the test file's directory"
    ,extension   = ".test" &= typ "EXT" &= help "Filename suffix of test files (default: .test)"
    ,with        = def     &= typ "EXECUTABLE" &= help "Replace the first word of (unindented) test commands"
    ,debug       = def     &= help "Show debug info, for troubleshooting"
    ,debug_parse = def     &= help "Show test file parsing info and stop"
    ,help_format = def     &= explicit &= name "help-format" &= help "Display test format help"
    ,testpaths   = def     &= args &= typ "TESTFILES|TESTDIRS"
    }
    &= program progname
    &= summary progversion
    &= details proghelpsuffix

data ShellTest = ShellTest {
     testname         :: String
    ,command          :: TestCommand
    ,stdin            :: Maybe String
    ,stdoutExpected   :: Maybe Matcher
    ,stderrExpected   :: Maybe Matcher
    ,exitCodeExpected :: Matcher
    }

data TestCommand = ReplaceableCommand String
                 | FixedCommand String
                   deriving Show

type Regexp = String

data Matcher = Lines Int String
             | Numeric String
             | NegativeNumeric String
             | PositiveRegex Regexp
             | NegativeRegex Regexp

main :: IO ()
main = do
  args' <- cmdArgs argdefs >>= checkArgs
  let (args,passthroughopts) = (args'{testpaths=realargs}, ptopts)
          where (ptopts,realargs) = partition ("-" `isPrefixOf`) $ testpaths args'
  when (debug args) $ printf "%s\n" progversion >> printf "args: %s\n" (show args)
  let paths = testpaths args
  testfiles' <- nub . concat <$> mapM (\p -> do
                                       isdir <- doesDirectoryExist p
                                       if isdir
                                        then findWithHandler (\_ e->fail (show e)) always (Find.extension ==? extension args) p
                                        else return [p]) paths
  let testfiles = filter (not . \p -> any (`isInfixOf` p) (exclude args)) testfiles'
      excluded = length testfiles' - length testfiles
  when (excluded > 0) $ printf "Excluding %d test files\n" excluded
  when (debug args) $ printf "processing %d test files: %s\n" (length testfiles) (intercalate ", " $ map fromPlatformString $ testfiles)
  parseresults <- mapM (parseShellTestFile args) testfiles
  unless (debug_parse args) $ defaultMainWithArgs
                               (concatMap (hUnitTestToTests . testFileParseToHUnitTest args) parseresults)
                               (passthroughopts ++ if color args then [] else ["--plain"])

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

-- parsing

parseShellTestFile :: Args -> FilePath -> IO (Either ParseError [ShellTest])
parseShellTestFile args f = do
  p <- parseFromFile shelltestfilep f
  case p of
    Right ts -> do
           let ts' | length ts > 1 = [t{testname=testname t++":"++show n} | (n,t) <- zip ([1..]::[Int]) ts]
                   | otherwise     = ts
           when (debug args || debug_parse args) $ do
                               printf "parsed %s:\n" $ fromPlatformString f
                               mapM_ (putStrLn.(' ':).show) ts'
           return $ Right ts'
    Left _ -> do
           when (debug args || debug_parse args) $ do
                               printf "failed to parse any tests in %s\n" $ fromPlatformString f
           return p

shelltestfilep :: Parser [ShellTest]
shelltestfilep = do
  ts <- many (try shelltestp)
  skipMany whitespaceorcommentlinep
  eof
  return ts

shelltestp :: Parser ShellTest
shelltestp = do
  st <- getParserState
  let f = fromPlatformString $ sourceName $ statePos st
  skipMany whitespaceorcommentlinep
  c <- commandp <?> "command line"
  i <- optionMaybe inputp <?> "input"
  o <- optionMaybe expectedoutputp <?> "expected output"
  e <- optionMaybe expectederrorp <?> "expected error output"
  x <- expectedexitcodep <?> "expected exit status"
  when (null (show c) && (isNothing i) && (null $ catMaybes [o,e]) && null (show x)) $ fail ""
  return $ ShellTest{testname=f,command=c,stdin=i,stdoutExpected=o,stderrExpected=e,exitCodeExpected=x}

newlineoreofp, whitespacecharp :: Parser Char
linep,lineoreofp,whitespacep,whitespacelinep,commentlinep,whitespaceorcommentlinep,whitespaceorcommentlineoreofp,delimiterp,inputp :: Parser String
linep = (anyChar `manyTill` newline) <?> "rest of line"
newlineoreofp = newline <|> (eof >> return '\n') <?> "newline or end of file"
lineoreofp = (anyChar `manyTill` newlineoreofp)
whitespacecharp = oneOf " \t"
whitespacep = many whitespacecharp
whitespacelinep = try (newline >> return "") <|> try (whitespacecharp >> whitespacecharp `manyTill` newlineoreofp)
commentlinep = try (whitespacep >> char '#' >> lineoreofp) <?> "comments"
whitespaceorcommentlinep = commentlinep <|> whitespacelinep
whitespaceorcommentlineoreofp = choice [(eof >> return ""), commentlinep, whitespacelinep]
delimiterp = choice [try $ string "<<<", try $ string ">>>", eof >> return ""]

commandp,fixedcommandp,replaceablecommandp :: Parser TestCommand
commandp = fixedcommandp <|> replaceablecommandp
fixedcommandp = many1 whitespacecharp >> linep >>= return . FixedCommand
replaceablecommandp = linep >>= return . ReplaceableCommand

inputp = try $ string "<<<" >> whitespaceorcommentlinep >> (liftM unlines) (linep `manyTill` (lookAhead delimiterp))

expectedoutputp :: Parser Matcher
expectedoutputp = (try $ do
  string ">>>" >> optional (char '1')
  whitespacep
  choice [positiveregexmatcherp, negativeregexmatcherp, whitespaceorcommentlineoreofp >> linesmatcherp]
 ) <?> "expected output"

expectederrorp :: Parser Matcher
expectederrorp = (try $ do
  string ">>>2"
  whitespacep
  choice [positiveregexmatcherp, negativeregexmatcherp, (whitespaceorcommentlineoreofp >> linesmatcherp)]
 ) <?> "expected error output"

expectedexitcodep :: Parser Matcher
expectedexitcodep = (try $ do
  string ">>>="
  whitespacep
  choice [positiveregexmatcherp, try negativeregexmatcherp, numericmatcherp, negativenumericmatcherp]
 ) <?> "expected exit status"

linesmatcherp :: Parser Matcher
linesmatcherp = do
  ln <- liftM sourceLine getPosition
  (liftM $ Lines ln . unlines) (linep `manyTill` (lookAhead delimiterp)) <?> "lines of output"

negativeregexmatcherp :: Parser Matcher
negativeregexmatcherp = (do
  char '!'
  PositiveRegex r <- positiveregexmatcherp
  return $ NegativeRegex r) <?> "non-matched regexp pattern"

positiveregexmatcherp :: Parser Matcher
positiveregexmatcherp = (do
  char '/'
  r <- (try escapedslashp <|> noneOf "/") `manyTill` (char '/')
  whitespaceorcommentlineoreofp
  return $ PositiveRegex r) <?> "regexp pattern"

negativenumericmatcherp :: Parser Matcher
negativenumericmatcherp = (do
  char '!'
  Numeric s <- numericmatcherp
  return $ NegativeNumeric s
  ) <?> "non-matched number"

numericmatcherp :: Parser Matcher
numericmatcherp = (do
  s <- many1 $ oneOf "0123456789"
  whitespaceorcommentlineoreofp
  return $ Numeric s
  ) <?> "number"

escapedslashp :: Parser Char
escapedslashp = char '\\' >> char '/'

-- running

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
  when (debug args) $ printf "command was: %s\n" (show cmd)
  (o_actual, e_actual, x_actual) <- runCommandWithInput dir (toPlatformString cmd) i
  when (debug args) $ do
    printf "stdout was : %s\n" (show $ trim' o_actual)
    printf "stderr was : %s\n" (show $ trim' e_actual)
    printf "exit was   : %s\n" (show $ trim' $ show x_actual)
  if (x_actual == 127) -- catch bad executable - should work on posix systems at least
   then ioError $ userError e_actual -- XXX still a test failure; should be an error
   else assertString $ addnewline $ intercalate "\n" $ filter (not . null) [
             if (maybe True (o_actual `matches`) o_expected)
              then ""
              else showExpectedActual args "stdout"    (fromJust o_expected) o_actual
            ,if (maybe True (e_actual `matches`) e_expected)
              then ""
              else showExpectedActual args "stderr"    (fromJust e_expected) e_actual
            ,if (show x_actual `matches` x_expected)
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

instance Show Matcher where show = showMatcherTrimmed

showMatcherTrimmed :: Matcher -> String
showMatcherTrimmed (PositiveRegex r)   = "/"++(trim r)++"/"
showMatcherTrimmed (NegativeRegex r)   = "!/"++(trim r)++"/"
showMatcherTrimmed (Numeric s)         = trim s
showMatcherTrimmed (NegativeNumeric s) = "!"++ trim s
showMatcherTrimmed (Lines _ s)         = trim s

showMatcher :: Matcher -> String
showMatcher (PositiveRegex r)   = "/"++r++"/"
showMatcher (NegativeRegex r)   = "!/"++r++"/"
showMatcher (Numeric s)         = s
showMatcher (NegativeNumeric s) = "!"++ s
showMatcher (Lines _ s)         = s

instance Show ShellTest where
    show ShellTest{testname=n,command=c,stdin=i,stdoutExpected=o,stderrExpected=e,exitCodeExpected=x} =
        printf "ShellTest {testname = %s, command = %s, stdin = %s, stdoutExpected = %s, stderrExpected = %s, exitCodeExpected = %s}"
                   (show $ trim n)
                   (show c)
                   (maybe "Nothing" (show.trim) i)
                   (show o)
                   (show e)
                   (show x)


-- utils

trim :: String -> String
trim s | l <= limit = s
       | otherwise = take limit s ++ suffix
    where
      limit = 500
      l = length s
      suffix = printf "...(%d more)" (l-limit)

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

-- | Test if a string contains a regular expression.  A malformed regexp
-- (or a regexp larger than 300 characters, to avoid a regex-tdfa memory leak)
-- will cause a runtime error.  This version uses regex-tdfa and no regexp
-- options.
containsRegex :: String -> String -> Bool
containsRegex s "" = containsRegex s "^"
containsRegex s r
    | length r <= 300 = s =~ r
    | otherwise      =  error "please avoid regexps larger than 300 characters, they are currently problematic"

-- | Replace occurrences of old list with new list within a larger list.
replace::(Eq a) => [a] -> [a] -> [a] -> [a]
replace [] _ = id
replace old new = replace'
    where
     replace' [] = []
     replace' l@(h:ts) = if old `isPrefixOf` l
                          then new ++ replace' (drop len l)
                          else h : replace' ts
     len = length old
