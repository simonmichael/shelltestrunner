-- Print tests in any of the supported formats.
-- Useful for debugging and for migrating between formats.
-- Issues:
--  converting v1 -> v2/v3
--   a >>>= 0 often gets converted to a >>>2 // or >2 //, when >= or nothing would be preferred (but semantically less accurate, therefore risky to choose automatically)
--  converting v3 -> v3
--   loses comments at the top of the file, even above an explicit < delimiter
--   may lose other data

module Print
  ( printShellTest
  , packResult
  )
where

import Import
import Types

-- | Print a shell test considering the @--actual=mode@ option. See CLI
-- documentation for details on.
-- For v3 (the preferred, lightweight format), avoid printing most unnecessary things
-- (stdout delimiter, 0 exit status value).
printShellTest
  :: String               -- ^ Shelltest format. Value of option @--print[=FORMAT]@.
  -> Maybe String         -- ^ Value of option @--actual[=MODE]@. @Nothing@ if option is not given.
  -> ShellTest            -- ^ Test to print
  -> Either String String -- ^ Actual stdout, non-matching or matching
  -> Either String String -- ^ Actual stderr, non-matching or matching
  -> Either Int Int       -- ^ Actual exit status, non-matching or matching
  -> IO ()
printShellTest format actualMode ShellTest{command=c,stdin=i,comments=comments,trailingComments=trailingComments,
               stdoutExpected=o_expected,stderrExpected=e_expected,exitCodeExpected=x_expected}
               o_actual e_actual x_actual = do
  (o,e,x) <- computeResults actualMode
  case format of
    "v1" -> do
      printComments comments
      printCommand "" c
      printStdin "<<<" i
      printStdouterr ">>>" $ justMatcherOutErr o
      printStdouterr ">>>2" $ justMatcherOutErr e
      printExitStatus True ">>>=" x
      printComments trailingComments
    "v2" -> do
      printComments comments
      printStdin "<<<" i
      printCommand "$$$ " c
      printStdouterr ">>>" $ justMatcherOutErr o
      printStdouterr ">>>2" $ justMatcherOutErr e
      printExitStatus False ">>>=" x
      printComments trailingComments
    "v3" -> do
      printComments comments
      printStdin "<" i
      printCommand "$ "  c
      printStdouterr ">" $ justMatcherOutErr o
      printStdouterr ">2" $ justMatcherOutErr e
      printExitStatus False ">=" x
      printComments trailingComments
    _ -> fail $ "Unsupported --print format: " ++ format
  where
    computeResults :: Maybe String -> IO (Maybe Matcher, Maybe Matcher, Matcher)
    computeResults Nothing = return (o_expected, e_expected, x_expected)
    computeResults (Just mode)
     | mode `isPrefixOf` "all" = return
         (Just $ Lines 0 $ fromEither o_actual
         ,Just $ Lines 0 $ fromEither e_actual
         ,Numeric $ show $ fromEither x_actual)
     | mode `isPrefixOf` "update" = return
         (either (Just . Lines 0) (const o_expected) o_actual
         ,either (Just . Lines 0) (const e_expected) e_actual
         ,either (Numeric . show) (const x_expected) x_actual)
     | otherwise = fail "Unsupported argument for --actual option. Allowed: all, update, or a prefix thereof."

printComments :: [String] -> IO ()
printComments = mapM_ putStrLn

printStdin :: String -> Maybe String -> IO ()
printStdin _ (Just "") = return ()
printStdin _ Nothing = return ()
printStdin prefix (Just s) = printf "%s\n%s" prefix s

printCommand :: String -> TestCommand -> IO ()
printCommand prefix (ReplaceableCommand s) = printf "%s%s\n" prefix s
printCommand prefix (FixedCommand s)       = printf "%s %s\n" prefix s

printStdouterr :: String -> Maybe Matcher -> IO ()
printStdouterr _ Nothing                    = return ()
printStdouterr _ (Just (Lines _ ""))        = return ()
printStdouterr _ (Just (Numeric _))         = fail "FATAL: Cannot handle Matcher (Numeric) for stdout/stderr."
printStdouterr _ (Just (NegativeNumeric _)) = fail "FATAL: Cannot handle Matcher (NegativeNumeric) for stdout/stderr."
printStdouterr ">" (Just (Lines _ s))       = printf "%s" s -- omit the optional ">" in format v3
printStdouterr prefix (Just (Lines _ s))    = printf "%s\n%s" prefix s
printStdouterr prefix (Just regex)          = printf "%s %s\n" prefix (show regex)


-- | Print an expected exit status clause, prefixed with the given delimiter.
-- First arg says 'alwaysPrintEvenIfZero'.
printExitStatus :: Bool -> String -> Matcher -> IO ()
printExitStatus _ _ (Lines _ _) = fail "FATAL: Cannot handle Matcher (Lines) for exit status."
printExitStatus False _     (Numeric "0") = return ()
printExitStatus True prefix (Numeric "0") = printf "%s 0\n" prefix
printExitStatus _ prefix s = printf "%s %s\n" prefix (show s)

-- | Wrap result @a@ into 'Either' depending on wether it matches the expected result.
packResult :: Bool -> a -> Either a a
packResult True = Right
packResult False = Left

fromEither :: Either a a -> a
fromEither = either id id

-- | Return the default 'Matcher' for 'Nothing'.
justMatcherOutErr :: Maybe Matcher -> Maybe Matcher
justMatcherOutErr = Just . fromMaybe (Lines 0 "")
