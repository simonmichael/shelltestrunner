module Types
where

import Import
import Utils
import Text.Parsec
import Data.String (IsString(..))

data ShellTest = ShellTest {
     comments         :: [String] -- # COMMENTS OR BLANK LINES before test
    ,trailingComments :: [String] -- # COMMENTS OR BLANK LINES after the last test
    ,command          :: TestCommand
    ,stdin            :: Maybe String
    ,stdoutExpected   :: Maybe Matcher
    ,stderrExpected   :: Maybe Matcher
    ,exitCodeExpected :: Matcher
    ,testname         :: String
    ,lineNumber       :: Int
    }

instance Show ShellTest where
    show ShellTest{testname=n,command=c,lineNumber=ln,stdin=i,stdoutExpected=o,stderrExpected=e,exitCodeExpected=x} =
        printf "ShellTest {command = %s, stdin = %s, stdoutExpected = %s, stderrExpected = %s, exitCodeExpected = %s, testname = %s, lineNumber = %s}"
                   (show c)
                   (maybe "Nothing" (show.trim) i)
                   (show $ show <$> o)
                   (show $ show <$> e)
                   (show x)
                   (show $ trim n)
                   (show ln)

data TestCommand = ReplaceableCommand String
                 | FixedCommand String
                   deriving Show

newtype MatcherRegex = MR String
instance IsString MatcherRegex where fromString = MR
instance Show MatcherRegex where show (MR r) = "/" ++ escapeMatcherRegex r ++ "/"
-- Do some minimal escaping to avoid showing unparseable regex matchers:
-- backslash-escape forward slashes.
-- XXX not super efficient
escapeMatcherRegex (c:r) = (if c == '/' then "\\/" else [c]) ++ escapeMatcherRegex r
escapeMatcherRegex r     = r

data Matcher = Lines Int String             -- ^ 0 or more lines of text, also the starting line number ?
             | Numeric String               -- ^ numeric exit code as a string
             | NegativeNumeric String       -- ^ numeric exit code as a string, matched negatively
             | PositiveRegex MatcherRegex   -- ^ regular expression
             | NegativeRegex MatcherRegex   -- ^ regular expression, matched negatively

instance Show Matcher where show = showMatcherTrimmed

showMatcherTrimmed :: Matcher -> String
showMatcherTrimmed (PositiveRegex (MR r)) = show $ MR $ trim r
showMatcherTrimmed (NegativeRegex mr)    = "!" ++ showMatcherTrimmed (PositiveRegex mr)
showMatcherTrimmed (Numeric s)          = trim s
showMatcherTrimmed (NegativeNumeric s)  = "!"++ trim s
showMatcherTrimmed (Lines _ s)          = trim s

showMatcher :: Matcher -> String
showMatcher mr@(PositiveRegex _) = show mr
showMatcher (NegativeRegex r)    = "!" ++ showMatcher (PositiveRegex r)
showMatcher (Numeric s)          = s
showMatcher (NegativeNumeric s)  = "!"++ s
showMatcher (Lines _ s)          = s

type Macro = (String, String)

data PreProcessor = NoPreprocess |
                    PreProcessor [(String -> Either ParseError String)]
