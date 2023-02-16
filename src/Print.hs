module Print
where

import Safe (lastMay)
import Import
import Types

-- | Print a shell test. See CLI documentation for details.
-- For v3 (the preferred, lightweight format), avoid printing most unnecessary things
-- (stdout delimiter, 0 exit status value).
printShellTest
  :: String               -- ^ Shelltest format. Value of option @--print[=FORMAT]@.
  -> ShellTest            -- ^ Test to print
  -> IO ()
printShellTest format ShellTest{command=c,stdin=i,comments=comments,trailingComments=trailingComments,
               stdoutExpected=o_expected,stderrExpected=e_expected,exitCodeExpected=x_expected}
               = do
  case format of
    "v1" -> do
      printComments comments
      printCommand "" c
      printStdin "<<<" i
      printStdouterr False ">>>" o_expected
      printStdouterr False ">>>2" e_expected
      printExitStatus True True ">>>=" x_expected
      printComments trailingComments
    "v2" -> do
      printComments comments
      printStdin "<<<" i
      printCommand "$$$ " c
      printStdouterr True ">>>" o_expected
      printStdouterr True ">>>2" e_expected
      printExitStatus trailingblanklines True ">>>=" x_expected
      printComments trailingComments
    "v3" -> do
      printComments comments
      printStdin "<" i
      printCommand "$ "  c
      printStdouterr True ">" o_expected
      printStdouterr True ">2" e_expected
      printExitStatus trailingblanklines False ">=" x_expected
      printComments trailingComments
    _ -> fail $ "Unsupported --print format: " ++ format
  where
    trailingblanklines = case (o_expected, e_expected) of
      (Just (Lines _ o), Just (Lines _ e)) -> hasblanks $ if null e then o else e
      _ -> False
      where hasblanks s = maybe False null $ lastMay $ lines s

printComments :: [String] -> IO ()
printComments = mapM_ putStrLn

printStdin :: String -> Maybe String -> IO ()
printStdin _ (Just "") = return ()
printStdin _ Nothing = return ()
printStdin prefix (Just s) = printf "%s\n%s" prefix s

printCommand :: String -> TestCommand -> IO ()
printCommand prefix (ReplaceableCommand s) = printf "%s%s\n" prefix s
printCommand prefix (FixedCommand s)       = printf "%s %s\n" prefix s

-- Print an expected stdout or stderr test, prefixed with the given delimiter.
-- If no expected value is specified, print nothing if first argument is true
-- (for format 1, which ignores unspecified out/err), otherwise print a dummy test.
printStdouterr :: Bool -> String -> Maybe Matcher -> IO ()
printStdouterr alwaystest prefix Nothing                 = when alwaystest $ printf "%s //\n" prefix
printStdouterr _ _ (Just (Lines _ ""))                   = return ()
printStdouterr _ _ (Just (Numeric _))                    = fail "FATAL: Cannot handle Matcher (Numeric) for stdout/stderr."
printStdouterr _ _ (Just (NegativeNumeric _))            = fail "FATAL: Cannot handle Matcher (NegativeNumeric) for stdout/stderr."
printStdouterr _ prefix (Just (Lines _ s)) | prefix==">" = printf "%s" s  -- omit v3's > delimiter, really no need for it
printStdouterr _ prefix (Just (Lines _ s))               = printf "%s\n%s" prefix s
printStdouterr _ prefix (Just regex)                     = printf "%s %s\n" prefix (show regex)

-- | Print an expected exit status clause, prefixed with the given delimiter.
-- If zero is expected:
--  if the first argument is not true, nothing will be printed;
--  otherwise if the second argument is not true, only the delimiter will be printed.
printExitStatus :: Bool -> Bool ->  String -> Matcher ->      IO ()
printExitStatus    _       _        _         (Lines _ _)   = fail "FATAL: Cannot handle Matcher (Lines) for exit status."
printExitStatus    always  showzero prefix    (Numeric "0") = when always $ printf "%s %s\n" prefix (if showzero then "0" else "")
printExitStatus    _       _        prefix    s             = printf "%s %s\n" prefix (show s)
