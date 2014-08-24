module Utils
where

import Debug.Trace
import Text.ParserCombinators.Parsec
import Text.Regex.TDFA ((=~))

import Import


-- | Trace (print on stdout at runtime) a showable value.
-- (for easily tracing in the middle of a complex expression)
strace :: Show a => a -> a
strace a = trace (show a) a

-- | Labelled trace - like strace, with a label prepended.
ltrace :: Show a => String -> a -> a
ltrace l a = trace (l ++ ": " ++ show a) a

-- | Monadic trace - like strace, but works as a standalone line in a monad.
mtrace :: (Monad m, Show a) => a -> m a
mtrace a = strace a `seq` return a

-- | Custom trace - like strace, with a custom show function.
traceWith :: (a -> String) -> a -> a
traceWith f e = trace (f e) e

-- | Parsec trace - show the current parsec position and next input,
-- and the provided label if it's non-null.
ptrace :: String -> GenParser Char st ()
ptrace msg = do
  pos <- getPosition
  next <- take peeklength `fmap` getInput
  let (l,c) = (sourceLine pos, sourceColumn pos)
      s  = printf "at line %2d col %2d: %s" l c (show next) :: String
      s' = printf ("%-"++show (peeklength+30)++"s") s ++ " " ++ msg
  trace s' $ return ()
  where
    peeklength = 30



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

choice' :: [GenParser tok st a] -> GenParser tok st a
choice' = choice . map Text.ParserCombinators.Parsec.try
