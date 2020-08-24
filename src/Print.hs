module Print
where

import Import
import Types

-- | Print a shell test considering the @--actual=mode@ option. See CLI
-- documentation for details on.
printShellTest
  :: String               -- ^ Shelltest format. Value of option @--print[=FORMAT]@.
  -> Maybe String         -- ^ Value of option @--actual[=MODE]@. @Nothing@ if option is not given.
  -> ShellTest            -- ^ Test to print
  -> Either String String -- ^ Non-matching or matching exit status
  -> Either String String -- ^ Non-matching or matching exit status
  -> Either Int Int       -- ^ Non-matching or matching exit status
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
  where
    computeResults :: Maybe String -> IO (Maybe Matcher, Maybe Matcher, Matcher)
    computeResults Nothing = do
          return (o_expected, e_expected, x_expected)
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
printStdouterr prefix (Just (Lines _ s))    = printf "%s\n%s" prefix s
printStdouterr prefix (Just regex)          = printf "%s %s\n" prefix (show regex)

-- | Print exit status. First arg says 'alwaysPrintEvenIfZero'.
printExitStatus :: Bool -> String -> Matcher -> IO ()
printExitStatus _ _ (Lines _ _) = fail "FATAL: Cannot handle Matcher (Lines) for exit status."
printExitStatus False _     (Numeric "0") = return ()
printExitStatus True prefix (Numeric "0") = printf "%s 0\n" prefix
printExitStatus _ prefix s = printf "%s %s\n" prefix (show s)

mkEither :: Bool -> a -> Either a a
mkEither True = Right
mkEither False = Left

fromEither :: Either a a -> a
fromEither = either id id

-- | Make a Matcher out of Nothing.
justMatcherOutErr :: Maybe Matcher -> Maybe Matcher
justMatcherOutErr = Just . fromMaybe (Lines 0 "")
