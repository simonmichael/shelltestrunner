module Utils
where

import Data.List
import System.Exit
import Text.Printf (printf)
import Text.Regex.TDFA ((=~))


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

