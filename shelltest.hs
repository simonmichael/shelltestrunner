#!/usr/bin/env runhaskell
{-# LANGUAGE DeriveDataTypeable #-}
{- |

shelltest - a tool for testing command-line programs.

See shelltestrunner.cabal.

(c) Simon Michael 2009-2011, released under GNU GPLv3 or later.

-}

module Main
where
-- encoding: we try to handle non-ascii content in test file paths, test
-- commands, and expected test results.  For this we assume 1. ghc 6.12+
-- and 2. that on unix, all file names/arguments/environment variables are
-- utf8 encoded
import Codec.Binary.UTF8.String as UTF8 (decodeString, encodeString, isUTF8Encoded)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (liftM,when,unless)
import Data.List
import Data.Maybe (isNothing,isJust,fromJust,catMaybes)
import qualified Test.HUnit (Test)
-- cmdargs: the impure method of writing annotations is susceptible to
-- over-optimisation by GHC - sometimes {-# OPTIONS_GHC -fno-cse #-} will be
-- required.
import System.Console.CmdArgs
import System.Exit
import System.Info (os)
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
import System.FilePath.FindCompat (findWithHandler, (==?), always)
import qualified System.FilePath.FindCompat as Find (extension)
import Control.Applicative ((<$>))
import Data.Algorithm.Diff

strace :: Show a => a -> a
strace a = trace (show a) a

version, progname, progversion :: String
version = "0.9.98" -- keep synced with cabal file
progname = "shelltest"
progversion = progname ++ " " ++ version
proghelpsuffix :: [String]
proghelpsuffix = [
   -- keep this bit synced with options width
   "     -- TFOPTIONS       Set extra test-framework options like -j/--threads,"
  ,"                        -t/--select-tests, -o/--timeout (use -- --help for"
  ,"                        a list.) Avoid spaces."
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
     color       :: Bool
    ,diff        :: Bool
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
     color       = def     &= help "Show colored output if your terminal supports it"
    ,diff        = def     &= name "d" &= help "Show diff of expected vs. actual when tests fail"
    ,exclude     = def     &= name "x" &= typ "STR" &= help "Exclude test files whose path contains STR"
    ,execdir     = def     &= help "Run tests from within the test file's directory"
    ,extension   = ".test" &= typ "EXT" &= help "Filename suffix of test files (default: .test)"
    ,with        = def     &= typ "EXECUTABLE" &= help "Replace the first word of (unindented) test commands"
    ,debug       = def     &= help "Show debug info, for troubleshooting"
    ,debug_parse = def     &= help "Show test file parsing info and stop"
    ,help_format = def     &= help "Display test format help"
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
                               (concatMap (hUnitTestToTests.testFileParseToHUnitTest args) parseresults)
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
  when (debug args) $ printf "command was: %s\n" (show cmd)
  (o_actual, e_actual, x_actual) <- runCommandWithInput dir (toPlatformString cmd) i
  when (debug args) $ do
    printf "stdout was : %s\n" (show $ trim o_actual)
    printf "stderr was : %s\n" (show $ trim e_actual)
    printf "exit was   : %s\n" (show $ trim $ show x_actual)
  if (x_actual == 127) -- catch bad executable - should work on posix systems at least
   then ioError $ userError e_actual -- XXX still a test failure; should be an error
   else assertString $ addnewline $ intercalate "\n" $ filter (not . null) [
             if (maybe True (o_actual `matches`) o_expected)
              then ""
              else showExpectedActual (diff args) "stdout"    (fromJust o_expected) o_actual
            ,if (maybe True (e_actual `matches`) e_expected)
              then ""
              else showExpectedActual (diff args) "stderr"    (fromJust e_expected) e_actual
            ,if (show x_actual `matches` x_expected)
              then ""
              else showExpectedActual False "exit code" x_expected (show x_actual)
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

showExpectedActual :: Bool -> String -> Matcher -> String -> String
showExpectedActual True _ (Lines ln e) a =
    printf "--- Expected\n+++ Got\n" ++ showDiff (1,ln) (getDiff (lines a) (lines e))
showExpectedActual _ field e a =
    printf "**Expected %s: %s\n**Got %s:      %s" field (show e) field (show $ trim a)

showDiff :: (Int,Int) -> [(DI, String)] -> String
showDiff _ []             = ""
showDiff (l,r) ((F, ln) : ds) =
  printf "+%4d " l ++ ln ++ "\n" ++ showDiff (l+1,r) ds
showDiff (l,r) ((S, ln) : ds) =
  printf "-%4d " r ++ ln ++ "\n" ++ showDiff (l,r+1) ds
showDiff (l,r) ((B, _ ) : ds) =
  showDiff (l+1,r+1) ds

instance Show Matcher where
    show (PositiveRegex r)   = "/"++(trim r)++"/"
    show (NegativeRegex r)   = "!/"++(trim r)++"/"
    show (Numeric s)         = show $ trim s
    show (NegativeNumeric s) = "!"++ show (trim s)
    show (Lines _ s)         = show $ trim s

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

-- | A platform string is a string value from or for the operating system,
-- such as a file path or command-line argument (or environment variable's
-- name or value ?). On some platforms (such as unix) these are not real
-- unicode strings but have some encoding such as UTF-8. This alias does
-- no type enforcement but aids code clarity.
type PlatformString = String

-- | Convert a possibly encoded platform string to a real unicode string.
-- We decode the UTF-8 encoding recommended for unix systems
-- (cf http://www.dwheeler.com/essays/fixing-unix-linux-filenames.html)
-- and leave anything else unchanged.
fromPlatformString :: PlatformString -> String
fromPlatformString s = if UTF8.isUTF8Encoded s then UTF8.decodeString s else s

-- | Convert a unicode string to a possibly encoded platform string.
-- On unix we encode with the recommended UTF-8
-- (cf http://www.dwheeler.com/essays/fixing-unix-linux-filenames.html)
-- and elsewhere we leave it unchanged.
toPlatformString :: String -> PlatformString
toPlatformString = case os of
                     "unix" -> UTF8.encodeString
                     "linux" -> UTF8.encodeString
                     "darwin" -> UTF8.encodeString
                     _ -> id
