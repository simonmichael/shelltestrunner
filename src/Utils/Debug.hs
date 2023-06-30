{-# LANGUAGE CPP #-}
module Utils.Debug
  ( module Debug.Trace
  , traceWith
  , strace
  , ltrace
  , mtrace
  , ptrace
  , ppShow
  , debugLevel
  , dbg
  , dbg0
  , dbg1
  , dbg2
  , dbg3
  , dbg4
  , dbg5
  , dbg6
  , dbg7
  , dbg8
  , dbg9
  , dbgAt
  , dbgAtIO
  -- , dbgAtM
  -- , dbgtrace
  -- , dbgshow
  -- , dbgppshow
  , dbgExit
  , pdbg
  )
  where

import Debug.Trace (trace)
import Control.Monad (when)
import Data.List
import Safe (readDef)
import System.Environment (getArgs)
import System.Exit
import System.IO.Unsafe (unsafePerformIO)
import Text.Parsec (getPosition, getInput, sourceLine, sourceColumn)
import Text.Parsec.String (GenParser)
import Text.Printf

-- ppShow is good at prettyifying many type's default show format
#if __GLASGOW_HASKELL__ >= 704
-- a good version of it requires GHC 7.4+
import Text.Show.Pretty (ppShow)
#else
-- otherwise just use show
ppShow :: Show a => a -> String
ppShow = show
#endif

-- | Trace a value using some string-producing function.
traceWith :: (a -> String) -> a -> a
traceWith f e = trace (f e) e

-- | Trace a showable value by showing it.
strace :: Show a => a -> a
strace = traceWith show

-- | Labelled trace - like strace, with a label prepended.
ltrace :: Show a => String -> a -> a
ltrace l a = trace (l ++ ": " ++ show a) a

-- | Monadic trace - like strace, but works as a standalone line in a monad.
mtrace :: (Monad m, Show a) => a -> m a
mtrace a = strace a `seq` return a

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

-- | Global debug level, which controls the verbosity of debug output
-- on the console. The default is 0 meaning no debug output. The
-- @--debug@ command line flag sets it to 1, or @--debug=N@ sets it to
-- a higher value (note: not @--debug N@ for some reason).  This uses
-- unsafePerformIO and can be accessed from anywhere and before normal
-- command-line processing. After command-line processing, it is also
-- available as the @debug_@ field of 'Hledger.Cli.Options.CliOpts'.
debugLevel :: Int
debugLevel = case snd $ break (=="--debug") args of
               "--debug":[]  -> 1
               "--debug":n:_ -> readDef 1 n
               _             ->
                 case take 1 $ filter ("--debug" `isPrefixOf`) args of
                   ['-':'-':'d':'e':'b':'u':'g':'=':v] -> readDef 1 v
                   _                                   -> 0

    where
      args = unsafePerformIO getArgs

-- | Print a message and a showable value to the console if the global
-- debug level is non-zero.  Uses unsafePerformIO.
dbg :: Show a => String -> a -> a
dbg = dbg1

-- always prints
dbg0 :: Show a => String -> a -> a
dbg0 = dbgAt 0

dbg1 :: Show a => String -> a -> a
dbg1 = dbgAt 1

dbg2 :: Show a => String -> a -> a
dbg2 = dbgAt 2

dbg3 :: Show a => String -> a -> a
dbg3 = dbgAt 3

dbg4 :: Show a => String -> a -> a
dbg4 = dbgAt 4

dbg5 :: Show a => String -> a -> a
dbg5 = dbgAt 5

dbg6 :: Show a => String -> a -> a
dbg6 = dbgAt 6

dbg7 :: Show a => String -> a -> a
dbg7 = dbgAt 7

dbg8 :: Show a => String -> a -> a
dbg8 = dbgAt 8

dbg9 :: Show a => String -> a -> a
dbg9 = dbgAt 9

-- | Print a message and a showable value to the console if the global
-- debug level is at or above the specified level.  Uses unsafePerformIO.
dbgAt :: Show a => Int -> String -> a -> a
dbgAt lvl = dbgppshow lvl

    -- Could not deduce (a ~ ())
    -- from the context (Show a)
    --   bound by the type signature for
    --              dbgM :: Show a => String -> a -> IO ()
    --   at hledger/Hledger/Cli/Main.hs:200:13-42
    --   ‘a’ is a rigid type variable bound by
    --       the type signature for dbgM :: Show a => String -> a -> IO ()
    --       at hledger/Hledger/Cli/Main.hs:200:13
    -- Expected type: String -> a -> IO ()
    --   Actual type: String -> a -> IO a
-- -- dbgAtM :: (Monad m, Show a) => Int -> String -> a -> m a
-- -- dbgAtM lvl lbl x = dbgAt lvl lbl x `seq` return x
-- -- XXX temporary:
-- dbgAtM :: Show a => Int -> String -> a -> IO ()
-- dbgAtM = dbgAtIO

dbgAtIO :: Show a => Int -> String -> a -> IO ()
dbgAtIO lvl lbl x = dbgAt lvl lbl x `seq` return ()

-- -- | print this string to the console before evaluating the expression,
-- -- if the global debug level is non-zero.  Uses unsafePerformIO.
-- dbgtrace :: String -> a -> a
-- dbgtrace
--     | debugLevel > 0 = trace
--     | otherwise      = flip const

-- -- | Print a showable value to the console, with a message, if the
-- -- debug level is at or above the specified level (uses
-- -- unsafePerformIO).
-- -- Values are displayed with show, all on one line, which is hard to read.
-- dbgshow :: Show a => Int -> String -> a -> a
-- dbgshow level
--     | debugLevel >= level = ltrace
--     | otherwise           = flip const

-- | Print a showable value to the console, with a message, if the
-- debug level is at or above the specified level (uses
-- unsafePerformIO).
-- Values are displayed with ppShow, each field/constructor on its own line.
dbgppshow :: Show a => Int -> String -> a -> a
dbgppshow level
    | debugLevel < level = flip const
    | otherwise = \s a -> let p = ppShow a
                              ls = lines p
                              nlorspace | length ls > 1 = "\n"
                                        | otherwise     = " " ++ take (10 - length s) (repeat ' ')
                              ls' | length ls > 1 = map (" "++) ls
                                  | otherwise     = ls
                          in trace (s++":"++nlorspace++intercalate "\n" ls') a

-- -- | Print a showable value to the console, with a message, if the
-- -- debug level is at or above the specified level (uses
-- -- unsafePerformIO).
-- -- Values are displayed with pprint. Field names are not shown, but the
-- -- output is compact with smart line wrapping, long data elided,
-- -- and slow calculations timed out.
-- dbgpprint :: Data a => Int -> String -> a -> a
-- dbgpprint level msg a
--     | debugLevel >= level = unsafePerformIO $ do
--                               pprint a >>= putStrLn . ((msg++": \n") ++) . show
--                               return a
--     | otherwise           = a


-- | Like dbg, then exit the program. Uses unsafePerformIO.
dbgExit :: Show a => String -> a -> a
dbgExit msg = const (unsafePerformIO exitFailure) . dbg msg

-- | Print a message and parsec debug info (parse position and next
-- input) to the console when the debug level is at or above
-- this level. Uses unsafePerformIO.
-- pdbgAt :: GenParser m => Float -> String -> m ()
pdbg level msg = when (level <= debugLevel) $ ptrace msg

