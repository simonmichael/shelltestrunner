#!/usr/bin/env runhaskell
{-# LANGUAGE DeriveDataTypeable #-}
{-

shelltestrunner - a tool for testing command-line programs.

See shelltestrunner.cabal.

(c) Simon Michael 2009, released under GNU GPLv3

-}

module Main
where
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (liftM,when,unless)
import Data.List (intercalate)
import Data.Maybe (isNothing,isJust,fromJust,maybe,catMaybes)
import qualified Test.HUnit (Test)
import System.Console.CmdArgs hiding (args)
import qualified System.Console.CmdArgs as CmdArgs (args)
import System.Exit (ExitCode(..), exitWith)
import System.IO (Handle, hGetContents, hPutStr)
import System.Process (runInteractiveCommand, waitForProcess)
import Test.Framework (defaultMainWithArgs)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit hiding (Test)
import Text.ParserCombinators.Parsec
import Text.Printf (printf)
import Text.Regex.PCRE.Light.Char8
import Debug.Trace
strace :: Show a => a -> a
strace a = trace (show a) a


version = "0.6.98" -- keep synced with shelltestrunner.cabal
progname = "shelltestrunner"
progversion = progname ++ " " ++ version
version, progname, progversion :: String

data Args = Args {
     debug      :: Bool
    ,debugparse :: Bool
    ,implicit   :: String
    ,executable :: String
    ,testfiles  :: [String]
    ,otheropts  :: [String]
    } deriving (Show, Data, Typeable)

nullargs :: Args
nullargs = Args False False "" "" [] []

argmodes :: [Mode Args]
argmodes = [
  mode $ Args{
            debug      = def &= flag "debug" & text "show debug messages"
           ,debugparse = def &= flag "debug-parse" & explicit & text "show debug messages and stop after parsing"
           ,implicit   = "exit" &= typ "none|exit|all" & text "provide implicit tests"
           ,executable = def &= argPos 0 & typ "EXECUTABLE" & text "executable under test"
           ,testfiles  = def &= CmdArgs.args & typ "TESTFILES" & text "test files"
           ,otheropts  = def &= unknownFlags & explicit & typ "FLAGS" & text "other flags are passed to test runner"
           }
 ]

checkArgs :: Args -> IO Args
checkArgs args = do
  when (not $ i `elem` ["none","exit","all"]) $
       warn $ printf "Bad -i/--implicit value %s, valid choices are: none, exit or all" $ show i
  return args
    where
      i = implicit args
      warn s = cmdArgsHelp s argmodes Text >>= putStrLn >> exitWith (ExitFailure 1)

data ShellTest = ShellTest {
     testname         :: String
    ,commandargs      :: String
    ,stdin            :: Maybe String
    ,stdoutExpected   :: Maybe Matcher
    ,stderrExpected   :: Maybe Matcher
    ,exitCodeExpected :: Maybe Matcher
    }

type Regexp = String

data Matcher = Lines String
             | Numeric String
             | NegativeNumeric String
             | PositiveRegex Regexp
             | NegativeRegex Regexp

main :: IO ()
main = do
  args <- cmdArgs progversion argmodes >>= checkArgs
  when (debug args) $ printf "args: %s\n" (show args)
  loud <- isLoud
  when loud $ do
         printf "executable: %s\n" (executable args)
         printf "test files: %s\n" (intercalate ", " $ testfiles args)
  shelltests <- liftM concat $ mapM (parseShellTestFile args) (testfiles args)
  unless (debugparse args) $
    defaultMainWithArgs (concatMap (hUnitTestToTests.shellTestToHUnitTest args) shelltests) (otheropts args)

-- parsing

parseShellTestFile :: Args -> FilePath -> IO [ShellTest]
parseShellTestFile args f = do
  ts <- liftM (either (error.show) id) $ parseFromFile shelltestfilep f
  let ts' | length ts > 1 = [t{testname=testname t++":"++show n} | (n,t) <- zip ([1..]::[Int]) ts]
          | otherwise     = ts
  when (debug args || debugparse args) $ do
    printf "parsed %s:\n" f
    mapM_ (putStrLn.(' ':).show) ts'
  return ts'

shelltestfilep :: Parser [ShellTest]
shelltestfilep = do
  ts <- many (try shelltestp)
  many commentlinep
  eof
  return ts

shelltestp :: Parser ShellTest
shelltestp = do
  st <- getParserState
  let f = sourceName $ statePos st
  many commentlinep
  c <- commandargsp <?> "command arguments"
  i <- optionMaybe inputp <?> "input"
  o <- optionMaybe expectedoutputp <?> "expected output"
  e <- optionMaybe expectederrorp <?> "expected error output"
  x <- optionMaybe expectedexitcodep <?> "expected exit status"
  when (null c && (isNothing i) && (null $ catMaybes [o,e,x])) $ fail "empty test file"
  return $ ShellTest{testname=f,commandargs=c,stdin=i,stdoutExpected=o,stderrExpected=e,exitCodeExpected=x}

newlineoreofp, whitespacecharp :: Parser Char
linep,lineoreofp,whitespacep,whitespacelinep,commentlinep,whitespaceorcommentlinep,whitespaceorcommentlineoreofp,commandargsp,delimiterp,inputp :: Parser String
linep = (anyChar `manyTill` newline) <?> "rest of line"
newlineoreofp = newline <|> (eof >> return '\n') <?> "newline or end of file"
lineoreofp = (anyChar `manyTill` newlineoreofp)
whitespacecharp = oneOf " \t"
whitespacep = many whitespacecharp
whitespacelinep = try (newline >> return "") <|> (whitespacecharp >> whitespacecharp `manyTill` newlineoreofp)
commentlinep = whitespacep >> char '#' >> lineoreofp <?> "comments"
whitespaceorcommentlinep = choice [try commentlinep, whitespacelinep]
whitespaceorcommentlineoreofp = choice [(eof >> return ""), try commentlinep, whitespacelinep]
delimiterp = choice [try $ string "<<<", try $ string ">>>", commentlinep >> return "", eof >> return ""]

commandargsp = linep

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

shellTestToHUnitTest :: Args -> ShellTest -> Test.HUnit.Test
shellTestToHUnitTest args ShellTest{testname=n,commandargs=c,stdin=i,stdoutExpected=o_expected,
                                    stderrExpected=e_expected,exitCodeExpected=x_expected} = 
 n ~: do
  let exe = executable args
      cmd = unwords [exe,c]
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
  when (debug args) $ printf "command: %s\n" (show cmd)
  (o_actual, e_actual, x_actual) <- runCommandWithInput cmd i
  when (debug args) $ do
    printf "stdout: %s\n" (show $ trim o_actual)
    printf "stderr: %s\n" (show $ trim e_actual)
    printf "exit:   %s\n" (show $ trim $ show x_actual)
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
  -- this has to be done carefully
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
hGetContentsStrictlyAnd h f = hGetContents h >>= \s -> length s `seq` f s

matches :: String -> Matcher -> Bool
matches s (PositiveRegex r)   = s `containsRegex` r
matches s (NegativeRegex r)   = not $ s `containsRegex` r
matches s (Numeric p)         = s == p
matches s (NegativeNumeric p) = not $ s == p
matches s (Lines p)           = s == p

showExpectedActual :: String -> Matcher -> String -> String
showExpectedActual field e a =
    printf "**Expected %s: %s\n**Got %s: %s" field (show e) field (show $ trim a)

instance Show Matcher where
    show (PositiveRegex r)   = "/"++(trim r)++"/"
    show (NegativeRegex r)   = "!/"++(trim r)++"/"
    show (Numeric s)         = show $ trim s
    show (NegativeNumeric s) = "!"++ show (trim s)
    show (Lines s)           = show $ trim s

instance Show ShellTest where
    show ShellTest{testname=n,commandargs=a,stdin=i,stdoutExpected=o,stderrExpected=e,exitCodeExpected=x} = 
        printf "ShellTest {testname = %s, commandargs = %s, stdin = %s, stdoutExpected = %s, stderrExpected = %s, exitCodeExpected = %s}"
                   (show $ trim n)
                   (show $ trim a)
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
