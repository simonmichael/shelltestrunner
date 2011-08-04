#!/usr/bin/env runhaskell
{-
website build script
ghc --make -Wall hakyll.hs && ./hakyll build
-}

import Control.Monad.Trans (liftIO)
import System.Process (system)
import Text.Printf
import Text.Hakyll
import Text.Pandoc

siteurl :: String
siteurl = "http://joyful.com/shelltestrunner"

main:: IO ()
main = hakyllWithConfiguration cfg $ do
    mapM_ page [
                "README.md"
               ]
    mapM_ static [
                 ]
    where
      -- Render a page and symlink it to the current directory.
      page p = do
        renderChain ["site.tmpl.hamlet"] $ createPage p
        liftIO $ system $ printf "[ -f %s ] || ln -s _site/%s" p' p'
            where p' = (reverse . dropWhile (/='.') . reverse) p ++ "html"

      -- hakyll config with custom pandoc config
      cfg :: HakyllConfiguration
      cfg = (defaultHakyllConfiguration siteurl) {
        -- additionalContext = Context, -- An additional context to use when rendering. This additional context is used globally.
        -- siteDirectory = FilePath, -- Directory where the site is placed.
        -- cacheDirectory = FilePath, -- Directory for cache files.
        enableIndexUrl = False, -- Enable index links.
        -- previewPollDelay = Int, -- Delay between polls in preview mode.
        pandocParserState = defaultParserState {
         -- stateParseRaw        = False, -- ^ Parse raw HTML and LaTeX?
         -- stateParserContext   = NullState, -- ^ Inside list?
         -- stateQuoteContext    = NoQuote,   -- ^ Inside quoted environment?
         -- stateSanitizeHTML    = False,     -- ^ Sanitize HTML?
         -- stateKeys            = [],        -- ^ List of reference keys
         -- stateNotes           = [],        -- ^ List of notes
         -- stateTabStop         = 4,         -- ^ Tab stop
         -- stateStandalone      = False,     -- ^ Parse bibliographic info?
         -- stateTitle           = [],        -- ^ Title of document
         -- stateAuthors         = [],        -- ^ Authors of document
         -- stateDate            = [],        -- ^ Date of document
         -- stateStrict          = False,     -- ^ Use strict markdown syntax?
         stateSmart           = False     -- ^ Use smart typography?
         -- stateLiterateHaskell = False,     -- ^ Treat input as literate haskell
         -- stateColumns         = 80,        -- ^ Number of columns in terminal
         -- stateHeaderTable     = [],        -- ^ Ordered list of header types used
         -- stateIndentedCodeClasses = []     -- ^ Classes to use for indented code blocks
        },
        pandocWriterOptions = defaultWriterOptions {
                                  -- so we can have a TOC:
         writerStandalone       = True, -- ^ Include header and footer
         writerTemplate         = pandocTemplate, -- ^ Template to use in standalone mode
         -- writerVariables        = [],    -- ^ Variables to set in template
         -- writerIncludeBefore    = "",    -- ^ Text to include before the body
         -- writerIncludeAfter     = "",    -- ^ Text to include after the body
         -- writerTabStop          = 4,     -- ^ Tabstop for conversion btw spaces and tabs
         writerTableOfContents  = True -- ^ Include table of contents
         -- writerS5               = False, -- ^ We're writing S5
         -- writerXeTeX            = False, -- ^ Create latex suitable for use by xetex
         -- writerHTMLMathMethod   = PlainMath, -- ^ How to print math in HTML
         -- writerIgnoreNotes      = False,     -- ^ Ignore footnotes (used in making toc)
         -- writerIncremental      = False,     -- ^ Incremental S5 lists
         -- writerNumberSections   = False,     -- ^ Number sections in LaTeX
         -- writerStrictMarkdown   = False,     -- ^ Use strict markdown syntax
         -- writerReferenceLinks   = False,     -- ^ Use reference links in writing markdown, rst
         -- writerWrapText         = True,      -- ^ Wrap text to line length
         -- writerLiterateHaskell  = False,     -- ^ Write as literate haskell
         -- writerEmailObfuscation = JavascriptObfuscation, -- ^ How to obfuscate emails
         -- writerIdentifierPrefix = "",                    -- ^ Prefix for section & note ids in HTML
        }
       }

      -- override pandoc's body html template to ensure the TOC can be enabled
      pandocTemplate = unlines
          [ "$if(title)$"
          , "<h1 class=\"title\">$title$</h1>"
          , "$endif$"
          , "$for(include-before)$"
          , "$include-before$"
          , "$endfor$"
          , "$if(toc)$"
          , "$toc$"
          , "$endif$"
          , "$body$"
          , "$for(include-after)$"
          , "$include-after$"
          , "$endfor$"
          ]
