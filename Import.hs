-- our local prelude, imports common to all modules

module Import
    ( module Control.Applicative
    , module Control.Monad
    , module Data.List
    , module Data.Maybe
    , module System.Exit
    , module Text.Printf
    ) where

import Control.Applicative ((<$>))
import Control.Monad (liftM, when, unless)
import Data.List
import Data.Maybe
import System.Exit
import Text.Printf (printf)
-- import Text.Regex.TDFA ((=~))
