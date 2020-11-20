module Print
where

import Import
import Types

-- | Print a shell test. See CLI documentation for details.
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
              printStdouterr ">>>" o_expected
              printStdouterr ">>>2" e_expected
              printExitStatus True ">>>=" x_expected
              printComments trailingComments
            "v2" -> do
              printComments comments
              printStdin "<<<" i
              printCommand "$$$ " c
              printStdouterr ">>>" o_expected
              printStdouterr ">>>2" e_expected
              printExitStatus False ">>>=" x_expected
              printComments trailingComments
            "v3" -> do
              printComments comments
              printStdin "<" i
              printCommand "$ "  c
              printStdouterr ">" o_expected
              printStdouterr ">2" e_expected
              printExitStatus False ">=" x_expected
              printComments trailingComments
            _ -> fail $ "Unsupported --print format: " ++ format

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
printStdouterr prefix (Just (Lines _ s))    = printf "%s\n%s" prefix s
printStdouterr prefix (Just regex)          = printf "%s %s\n" prefix (show regex)

-- | Print exit status. First arg says 'alwaysPrintEvenIfZero'.
printExitStatus :: Bool -> String -> Matcher -> IO ()
printExitStatus _ _ (Lines _ _) = fail "FATAL: Cannot handle Matcher (Lines) for exit status."
printExitStatus False _     (Numeric "0") = return ()
printExitStatus True prefix (Numeric "0") = printf "%s 0\n" prefix
printExitStatus _ prefix s = printf "%s %s\n" prefix (show s)
