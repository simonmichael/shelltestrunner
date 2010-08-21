#!/usr/bin/env runhaskell
{-# LANGUAGE DeriveDataTypeable #-}
{-

shelltest - a tool for testing command-line programs.

See shelltestrunner.cabal.

(c) Simon Michael 2009-2010, released under GNU GPLv3

-}

module Main
where
import Codec.Binary.UTF8.String (decodeString, encodeString, isUTF8Encoded)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (liftM,when,unless)
import Data.List (intercalate, nub, isPrefixOf)
import Data.Maybe (isNothing,isJust,fromJust,catMaybes)
import qualified Test.HUnit (Test)
import System.Console.CmdArgs hiding (args)
import qualified System.Console.CmdArgs as CmdArgs (args)
import System.Exit (ExitCode(..), exitWith)
import System.IO (Handle, hGetContents, hPutStr)
import System.Process (StdStream (CreatePipe), shell, createProcess, CreateProcess (..), waitForProcess, ProcessHandle)
import Test.Framework (defaultMainWithArgs)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit hiding (Test)
import Text.ParserCombinators.Parsec
import Text.Printf (printf)
import Text.Regex.PCRE.Light.Char8
import Debug.Trace
import System.Directory (doesDirectoryExist)
import System.FilePath (takeDirectory)
import System.FilePath.FindCompat (findWithHandler, (==?), always)
import qualified System.FilePath.FindCompat as Find (extension)
import Control.Applicative ((<$>))
strace :: Show a => a -> a
strace a = trace (show a) a


version = "0.8.99" -- keep synced with cabal file
progname = "shelltest"
progversion = progname ++ " " ++ version
version, progname, progversion :: String

data Args = Args {
     debug      :: Bool
    ,debugparse :: Bool
    ,color      :: Bool
    ,execdir    :: Bool
    ,extension  :: String
    ,implicit   :: String
    ,with       :: String
    ,testpaths  :: [FilePath]
    ,otheropts  :: [String]
    } deriving (Show, Data, Typeable)


nullargs :: Args
nullargs = Args False False False False "" "" "" [] []

argmodes :: [Mode Args]
argmodes = [
  mode (Args{
            debug      = def     &= text "show debug messages"
           ,debugparse = def     &= flag "debug-parse" & explicit & text "show parsing debug messages and stop"
           ,color      = def     &= text "display with ANSI color codes"
           ,execdir    = def     &= text "run tests in same directory as test file"
           ,extension  = ".test" &= typ "EXT" & text "extension of test files when dirs specified"
           ,implicit   = "exit"  &= typ "none|exit|all" & text "provide implicit tests"
           ,with       = def     &= typ "EXECUTABLE" & text "alternate executable, replaces the first word of test commands"
           ,testpaths  = def     &= CmdArgs.args & typ "TESTFILES|TESTDIRS" & text "test files or directories"
           ,otheropts  = def     &= unknownFlags & explicit & typ "OTHER FLAGS" & text "any other flags are passed to test runner"
           }) &= helpSuffix [
   "A test file contains one or more shell tests, which look like this:"
  ,""
  ," # optional comment lines"
  ," a one-line shell command to be tested"
  ," <<<"
  ," stdin lines"
  ," >>> [/regexp to match in stdout/]"
  ," [or expected stdout lines"
  ," >>>2 [/regexp to match in stderr/]"
  ," [or expected stderr lines]"
  ," >>>= expected exit status or /regexp/"
  ,""
  ,"The command line is required; all other fields are optional."
  ,"The expected stdout (>>>) and expected stderr (>>>2) fields can have either"
  ,"a regular expression match pattern, in which case the test passes if the"
  ,"output is matched, or 0 or more data lines, in which case the output"
  ,"must match these exactly. The expected exit status (>>>=) field can have"
  ,"either a numeric exit code or a /regexp/. A ! preceding a /regexp/ or exit"
  ,"code negates the match. The regular expression syntax is that of the"
  ,"pcre-light library with the dotall flag."
  ,""
  ,"By default there is an implicit test for exit status=0, but no implicit test"
  ,"for stdout or stderr.  You can change this with -i/--implicit-tests."
  ,""
  ,"The command runs in your current directory unless you use --execdir."
  ,"You can use --with/-w to replace the first word of command lines"
  ,"(everything up to the first space) with something else, eg to test a"
  ,"different version of your program. To prevent this, start the command line"
  ,"with a space."
  ,""
  ,"Any unrecognised options will be passed through to test-framework's runner."
  ,"You may be able to get a big speedup by running tests in parallel: try -j8."
  ]
 ]

checkArgs :: Args -> IO Args
checkArgs args = do
  when (not $ i `elem` ["none","exit","all"]) $
       warn $ printf "Bad -i/--implicit value %s, valid choices are: none, exit or all" $ show i
  when (null ps) $
       warn $ printf "Please specify at least one file or directory, eg: %s ." progname
  return args
    where
      i = implicit args
      ps = testpaths args

-- | Show a message, usage string, and terminate with exit status 1.
warn :: String -> IO ()
warn s = cmdArgsHelp s argmodes Text >>= putStrLn >> exitWith (ExitFailure 1)

data ShellTest = ShellTest {
     testname         :: String
    ,command          :: TestCommand
    ,stdin            :: Maybe String
    ,stdoutExpected   :: Maybe Matcher
    ,stderrExpected   :: Maybe Matcher
    ,exitCodeExpected :: Maybe Matcher
    }

data TestCommand = ReplaceableCommand String
                 | FixedCommand String
                   deriving Show

type Regexp = String

data Matcher = Lines String
             | Numeric String
             | NegativeNumeric String
             | PositiveRegex Regexp
             | NegativeRegex Regexp

-- I believe that as of ghc 6.12, file paths and arguments are utf-8 (or
-- any local encoding ?) on unix and unicode on windows. Hopefully this
-- decode function is safe to use on any string and will be helpful on
-- utf-8 unix systems.
decodeIfUTF8 :: String -> String
decodeIfUTF8 s = if isUTF8Encoded s then decodeString s else s

main :: IO ()
main = do
  args <- cmdArgs progversion argmodes >>= checkArgs
  when (debug args) $ printf "args: %s\n" (show args)
  let paths = testpaths args
  testfiles <- nub . concat <$> mapM (\p -> do
                                       isdir <- doesDirectoryExist p
                                       if isdir
                                        then findWithHandler (\_ e->fail (show e)) always (Find.extension ==? extension args) p
                                        else return [p]) paths
  verbose <- isLoud
  when verbose $ do
         printf "executable: %s\n" (decodeIfUTF8 $ with args)
         printf "test files: %s\n" (intercalate ", " $ map decodeIfUTF8 $ testfiles)
  parseresults <- mapM (parseShellTestFile args) testfiles
  unless (debugparse args) $
    defaultMainWithArgs (concatMap (hUnitTestToTests.testFileParseToHUnitTest args) parseresults) (otheropts args ++ if color args then [] else ["--plain"])

-- parsing

parseShellTestFile :: Args -> FilePath -> IO (Either ParseError [ShellTest])
parseShellTestFile args f = do
  p <- parseFromFile shelltestfilep f
  case p of
    Right ts -> do
           let ts' | length ts > 1 = [t{testname=testname t++":"++show n} | (n,t) <- zip ([1..]::[Int]) ts]
                   | otherwise     = ts
           when (debug args || debugparse args) $ do
                               printf "parsed %s:\n" $ decodeIfUTF8 f
                               mapM_ (putStrLn.(' ':).show) ts'
           return $ Right ts'
    Left _ -> return p

shelltestfilep :: Parser [ShellTest]
shelltestfilep = do
  ts <- many (try shelltestp)
  many commentlinep
  eof
  return ts

shelltestp :: Parser ShellTest
shelltestp = do
  st <- getParserState
  let f = decodeIfUTF8 $ sourceName $ statePos st
  many $ try commentlinep
  c <- commandp <?> "command line"
  i <- optionMaybe inputp <?> "input"
  o <- optionMaybe expectedoutputp <?> "expected output"
  e <- optionMaybe expectederrorp <?> "expected error output"
  x <- optionMaybe expectedexitcodep <?> "expected exit status"
  when (null (show c) && (isNothing i) && (null $ catMaybes [o,e,x])) $ fail ""
  return $ ShellTest{testname=f,command=c,stdin=i,stdoutExpected=o,stderrExpected=e,exitCodeExpected=x}

newlineoreofp, whitespacecharp :: Parser Char
linep,lineoreofp,whitespacep,whitespacelinep,commentlinep,whitespaceorcommentlinep,whitespaceorcommentlineoreofp,delimiterp,inputp :: Parser String
linep = (anyChar `manyTill` newline) <?> "rest of line"
newlineoreofp = newline <|> (eof >> return '\n') <?> "newline or end of file"
lineoreofp = (anyChar `manyTill` newlineoreofp)
whitespacecharp = oneOf " \t"
whitespacep = many whitespacecharp
whitespacelinep = try (newline >> return "") <|> (whitespacecharp >> whitespacecharp `manyTill` newlineoreofp)
commentlinep = whitespacep >> char '#' >> lineoreofp <?> "comments"
whitespaceorcommentlinep = choice [try commentlinep, whitespacelinep]
whitespaceorcommentlineoreofp = choice [(eof >> return ""), try commentlinep, whitespacelinep]
delimiterp = choice [try $ string "<<<", try $ string ">>>", try commentlinep >> return "", eof >> return ""]

commandp,fixedcommandp,replaceablecommandp :: Parser TestCommand
commandp = fixedcommandp <|> replaceablecommandp
fixedcommandp = space >> linep >>= return . FixedCommand
replaceablecommandp = linep >>= return . ReplaceableCommand

inputp = string "<<<" >> whitespaceorcommentlinep >> (liftM unlines) (linep `manyTill` (lookAhead delimiterp))

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
  (liftM $ Lines . unlines) (linep `manyTill` (lookAhead delimiterp)) <?> "lines of output"

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
      (o_expected',e_expected',x_expected') =
          case (implicit args) of
            "all"    -> (case o_expected of
                        Just m -> Just m
                        _ -> Just $ Lines ""
                     ,case e_expected of Just m -> Just m
                                         _      -> Just $ Lines  ""
                     ,case x_expected of Just m -> Just m
                                         _      -> Just $ Numeric "0")
            "exit" -> (o_expected,e_expected
                     ,case x_expected of Just m -> Just m
                                         _      -> Just $ Numeric "0")
            _ -> (o_expected,e_expected,x_expected)
      dir = if execdir args then Just $ takeDirectory n else Nothing
  when (debug args) $ printf "command was: %s\n" (show cmd)
  (o_actual, e_actual, x_actual) <- runCommandWithInput dir (encodeString cmd) i
  when (debug args) $ do
    printf "stdout was : %s\n" (show $ trim o_actual)
    printf "stderr was : %s\n" (show $ trim e_actual)
    printf "exit was   : %s\n" (show $ trim $ show x_actual)
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
matches s (Lines p)           = s == p

showExpectedActual :: String -> Matcher -> String -> String
showExpectedActual field e a =
    printf "**Expected %s: %s\n**Got %s:      %s" field (show e) field (show $ trim a)

instance Show Matcher where
    show (PositiveRegex r)   = "/"++(trim r)++"/"
    show (NegativeRegex r)   = "!/"++(trim r)++"/"
    show (Numeric s)         = show $ trim s
    show (NegativeNumeric s) = "!"++ show (trim s)
    show (Lines s)           = show $ trim s

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

-- | Does string contain this regular expression ?
-- A malformed regexp will cause a runtime error.
containsRegex :: String -> String -> Bool
containsRegex s r =
    case compileM r [
              dotall
             -- ,utf8  -- shows no obvious benefit with 6.10, review situation with 6.12
             ] of
      Right regex -> isJust $ match regex s []
      Left e -> error $ printf "bad regexp, %s: %s" e (trim $ r)

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
