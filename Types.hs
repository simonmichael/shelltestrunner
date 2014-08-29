module Types
where

import Import
import Utils

  
data ShellTest = ShellTest {
     command          :: TestCommand
    ,stdin            :: Maybe String
    ,stdoutExpected   :: Maybe Matcher
    ,stderrExpected   :: Maybe Matcher
    ,exitCodeExpected :: Matcher
    ,testname         :: String
    }

instance Show ShellTest where
    show ShellTest{testname=n,command=c,stdin=i,stdoutExpected=o,stderrExpected=e,exitCodeExpected=x} =
        printf "ShellTest {command = %s, stdin = %s, stdoutExpected = %s, stderrExpected = %s, exitCodeExpected = %s, testname = %s}"
                   (show c)
                   (maybe "Nothing" (show.trim) i)
                   (show $ show <$> o)
                   (show $ show <$> e)
                   (show x)
                   (show $ trim n)

data TestCommand = ReplaceableCommand String
                 | FixedCommand String
                   deriving Show

type Regexp = String

data Matcher = Lines Int String       -- ^ 0 or more lines of text, also the starting line number ?
             | Numeric String         -- ^ numeric exit code as a string
             | NegativeNumeric String -- ^ numeric exit code as a string, matched negatively
             | PositiveRegex Regexp   -- ^ regular expression
             | NegativeRegex Regexp   -- ^ regular expression, matched negatively

instance Show Matcher where show = showMatcherTrimmed

showMatcherTrimmed :: Matcher -> String
showMatcherTrimmed (PositiveRegex r)   = "/"++(trim r)++"/"
showMatcherTrimmed (NegativeRegex r)   = "!/"++(trim r)++"/"
showMatcherTrimmed (Numeric s)         = trim (show s)
showMatcherTrimmed (NegativeNumeric s) = "!"++ trim (show s)
showMatcherTrimmed (Lines _ s)         = trim s

showMatcher :: Matcher -> String
showMatcher (PositiveRegex r)   = "/"++r++"/"
showMatcher (NegativeRegex r)   = "!/"++r++"/"
showMatcher (Numeric s)         = s
showMatcher (NegativeNumeric s) = "!"++ s
showMatcher (Lines _ s)         = s

