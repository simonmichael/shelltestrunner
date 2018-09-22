module Parse
  ( parseShellTestFile
  )
  where

import Text.Parsec
import Text.Parsec.String

import Import
import Types
import Utils hiding (dbg, ptrace)
import qualified Utils
import Preprocessor
import System.IO hiding (stdin)


parseFromFileWithPreprocessor :: (Parser [ShellTest]) -> PreProcessor -> FilePath -> IO (Either ParseError [ShellTest])
parseFromFileWithPreprocessor p preproc fname =
    do
        h <- openFile fname ReadMode
        hSetNewlineMode h universalNewlineMode
        input <- hGetContents h
        let processed = preprocess preproc input
        case processed of    Right text -> return (runParser p () fname text)
                             (Left err) -> return (Left err) -- To make haskell happy.


-- parseFromFile
-- | Parse this shell test file, optionally logging debug output.
parseShellTestFile :: Bool -> PreProcessor -> FilePath -> IO (Either ParseError [ShellTest])
parseShellTestFile debug preProcessor f = do
  p <- parseFromFileWithPreprocessor shelltestfile preProcessor f
  case p of
    Right ts -> do
           let ts' | length ts > 1 = [t{testname=testname t++":"++show n} | (n,t) <- zip ([1..]::[Int]) ts]
                   | otherwise     = ts
           when (debug) $ do
             printf "parsed %s:\n" f
             mapM_ (putStrLn.(' ':).ppShow) ts'
           return $ Right ts'
    Left _ -> do
           when (debug) $ printf "failed to parse any tests in %s\n" f
           return p


-- parsers

-- show lots of parsing debug output if enabled.
-- NB not connected to the --debug-parse flag
showParserDebugOutput = debugLevel >= 2

ptrace_ s  | showParserDebugOutput = Utils.ptrace s
           | otherwise             = return ()
ptrace s a | showParserDebugOutput = Utils.ptrace $ s ++ ": " ++ show a
           | otherwise             = return ()

shelltestfile :: Parser [ShellTest]
shelltestfile = do
  ptrace_ "shelltestfile 0"
  ts <- try (do first <- try (format2testgroup False)
                rest <- many $ try (format2testgroup True)
                return $ concat $ first : rest
        )
        <|>
        (do first <- try (format3testgroup False)
            rest <- many $ try (format3testgroup True)
            return $ concat $ first : rest
        )
        <|>
        (many $ try format1test)
  ptrace_ "shelltestfile 1"
  skipMany whitespaceorcommentline
  ptrace_ "shelltestfile 2"
  eof
  ptrace "shelltestfile ." ts
  return ts


----------------------------------------------------------------------
-- format 1 (shelltestrunner 1.x)
-- each test specifies its input; missing test parts are not checked

format1test = do
  ptrace_ " format1test 0"
  skipMany whitespaceorcommentline
  ptrace_ " format1test 1"
  ln <- sourceLine <$> getPosition
  c <- command1 <?> "command line"
  ptrace " format1test c" c
  i <- optionMaybe input1 <?> "input"
  ptrace " format1test i" i
  o <- optionMaybe expectedoutput1 <?> "expected output"
  ptrace " format1test o" o
  e <- optionMaybe expectederror1 <?> "expected error output"
  ptrace " format1test e" e
  x <- expectedexitcode1 <?> "expected exit status"
  ptrace " format1test x" x
  when (null (show c) && (isNothing i) && (null $ catMaybes [o,e]) && null (show x)) $ fail ""
  f <- sourceName . statePos <$> getParserState
  let t = ShellTest{testname=f,command=c,stdin=i,stdoutExpected=o,stderrExpected=e,exitCodeExpected=x,lineNumber=ln}
  ptrace " format1test ." t
  return t

command1 :: Parser TestCommand
command1 = fixedcommand <|> replaceablecommand

input1 :: Parser String
input1 = try $ string "<<<" >> whitespaceorcommentline >> unlines <$> (line `manyTill` (lookAhead (try delimiter)))

expectedoutput1 :: Parser Matcher
expectedoutput1 = (try $ do
  string ">>>"
  whitespace
  choice [regexmatcher, negativeregexmatcher, whitespaceorcommentlineoreof >> linesmatcher1]
 ) <?> "expected output"

expectederror1 :: Parser Matcher
expectederror1 = (try $ do
  string ">>>2"
  whitespace
  choice [regexmatcher, negativeregexmatcher, (whitespaceorcommentlineoreof >> linesmatcher1)]
 ) <?> "expected error output"

expectedexitcode1 :: Parser Matcher
expectedexitcode1 = (try $ do
  string ">>>="
  whitespace
  fromMaybe anyMatcher <$> (optionMaybe $ choice [regexmatcher, try negativeregexmatcher, numericmatcher, negativenumericmatcher])
 ) <?> "expected exit status"

----------------------------------------------------------------------
-- format 2
-- input first, then tests; missing test parts are implicitly checked

-- A format 2 test group, optionally with the initial <<< delimiter
-- being optional.
format2testgroup :: Bool -> Parser [ShellTest]
format2testgroup inputRequiresDelimiter = do
  ptrace " format2testgroup 0" inputRequiresDelimiter
  skipMany whitespaceorcommentline
  ptrace_ " format2testgroup 1"
  let startdelims | inputRequiresDelimiter = ["<<<"]
                  | otherwise              = ["<<<", ""]
      enddelims = ["$$$","<<<"]
  i <- optionMaybe (linesBetween startdelims enddelims) <?> "input"
  ptrace " format2testgroup i" i
  ts <- many1 $ try (format2test i)
  ptrace " format2testgroup ." ts
  return ts

format2test :: Maybe String -> Parser ShellTest
format2test i = do
  ptrace_ "  format2test 0"
  skipMany whitespaceorcommentline
  ptrace_ "  format2test 1"
  ln <- sourceLine <$> getPosition
  c <- command2 <?> "command line"
  ptrace "  format2test c" c
  nullstdout <- nullLinesMatcher . sourceLine <$> getPosition
  o <- maybe (Just nullstdout) Just <$> optionMaybe expectedoutput2 <?> "expected output"
  ptrace "  format2test o" o
  nullstderr <- nullLinesMatcher . sourceLine <$> getPosition
  e <- maybe (Just $ nullstderr) Just <$> optionMaybe expectederror2 <?> "expected error output"
  ptrace "  format2test e" e
  x <- fromMaybe nullStatusMatcher <$> optionMaybe expectedexitcode2 <?> "expected exit status"
  ptrace "  format2test x" x
  when (null (show c) && (isNothing i) && (null $ catMaybes [o,e]) && null (show x)) $ fail ""
  f <- sourceName . statePos <$> getParserState
  let t = ShellTest{testname=f,command=c,stdin=i,stdoutExpected=o,stderrExpected=e,exitCodeExpected=x,lineNumber=ln}
  ptrace "  format2test ." t
  return t

nullLinesMatcher n = Lines n ""

nullStatusMatcher  = Numeric "0"

anyMatcher = PositiveRegex ""

command2 :: Parser TestCommand
command2 = string "$$$" >> optional (char ' ') >> (fixedcommand <|> replaceablecommand)

-- In format 2, >>> is used only with /REGEX/, also don't consume the
-- whitespace/comments immediately preceding a following test.
expectedoutput2 :: Parser Matcher
expectedoutput2 = (try $ do
  try (string ">>>" >> whitespace >> (regexmatcher <|> negativeregexmatcher))
  <|> (optional (string ">>>" >> whitespaceline) >> linesmatcher2 <?> "expected output")
 )

-- Don't consume the whitespace/comments immediately preceding a
-- following test.
expectederror2 :: Parser Matcher
expectederror2 = (try $ do
  ptrace_ "   expectederror2 0"
  string ">>>2" >> whitespace
  ptrace_ "   expectederror2 1"
  m <- (regexmatcher <|> negativeregexmatcher)
       <|>
       (newline >> linesmatcher2 <?> "expected error output")
  ptrace "   expectederror2 ." m
  return m
 )

expectedexitcode2 :: Parser Matcher
expectedexitcode2 = expectedexitcode1

-- The format 2 lines matcher consumes lines until one of these:
-- 1. another section delimiter in this test (>>>, >>>2, >>>=)
-- 2. the next test's start delimiter (<<<, $$$), or the start of blank/comment lines preceding it
-- 3. end of file, or the start of blank/comment lines preceding it
linesmatcher2 :: Parser Matcher
linesmatcher2 = do
  ptrace_ "    linesmatcher2 0"
  ln <- sourceLine <$> getPosition
  ls <- unlines <$>
        line `manyTill` lookAhead (choice' [delimiterNotNewTest
                                           ,many whitespaceorcommentline >> delimiterNewTest
                                           ,many whitespaceorcommentline >> eofasstr])
        <?> "lines of output"
  ptrace "    linesmatcher2 ." ls
  return $ Lines ln ls

delimiterNewTest = do
  -- ptrace_ "     delimiterNewTest 0"
  choice [string "$$$", string "<<<"]

delimiterNotNewTest = do
  -- ptrace_ "    delimiterNotNewTest 0"
  choice [try (string ">>>2"), try (string ">>>="), string ">>>"]

----------------------------------------------------------------------
-- format 3
-- Like format 2 but with short delimiters.
-- XXX duplication
format3testgroup :: Bool -> Parser [ShellTest]
format3testgroup inputRequiresDelimiter = do
  ptrace " format3testgroup 0" inputRequiresDelimiter
  skipMany whitespaceorcommentline
  ptrace_ " format3testgroup 1"
  let startdelims | inputRequiresDelimiter = ["<"]
                  | otherwise              = ["<", ""]
      enddelims = ["$","<"]
  i <- optionMaybe (linesBetween startdelims enddelims) <?> "input"
  ptrace " format3testgroup i" i
  ts <- many1 $ try (format3test i)
  ptrace " format3testgroup ." ts
  return ts

format3test :: Maybe String -> Parser ShellTest
format3test i = do
  ptrace_ "  format3test 0"
  skipMany whitespaceorcommentline
  ptrace_ "  format3test 1"
  ln <- sourceLine <$> getPosition
  c <- command3 <?> "command line"
  ptrace "  format3test c" c
  nullstdout <- nullLinesMatcher . sourceLine <$> getPosition
  o <- maybe (Just nullstdout) Just <$> optionMaybe expectedoutput3 <?> "expected output"
  ptrace "  format3test o" o
  nullstderr <- nullLinesMatcher . sourceLine <$> getPosition
  e <- maybe (Just $ nullstderr) Just <$> optionMaybe expectederror3 <?> "expected error output"
  ptrace "  format3test e" e
  x <- fromMaybe nullStatusMatcher <$> optionMaybe expectedexitcode3 <?> "expected exit status"
  ptrace "  format3test x" x
  when (null (show c) && (isNothing i) && (null $ catMaybes [o,e]) && null (show x)) $ fail ""
  f <- sourceName . statePos <$> getParserState
  let t = ShellTest{testname=f,command=c,stdin=i,stdoutExpected=o,stderrExpected=e,exitCodeExpected=x,lineNumber=ln}
  ptrace "  format3test ." t
  return t

command3 :: Parser TestCommand
command3 = string "$" >> optional (char ' ') >> (fixedcommand <|> replaceablecommand)

-- In format 3, >>> is used only with /REGEX/, also don't consume the
-- whitespace/comments immediately preceding a following test.
expectedoutput3 :: Parser Matcher
expectedoutput3 = (try $ do
  try (string ">" >> whitespace >> (regexmatcher <|> negativeregexmatcher))
  <|> (optional (string ">" >> whitespaceline) >> linesmatcher3 <?> "expected output")
 )

-- Don't consume the whitespace/comments immediately preceding a
-- following test.
expectederror3 :: Parser Matcher
expectederror3 = (try $ do
  ptrace_ "   expectederror3 0"
  string ">2" >> whitespace
  ptrace_ "   expectederror3 1"
  m <- (regexmatcher <|> negativeregexmatcher)
       <|>
       (newline >> linesmatcher3 <?> "expected error output")
  ptrace "   expectederror3 ." m
  return m
 )

expectedexitcode3 :: Parser Matcher
expectedexitcode3 = (try $ do
  string ">="
  whitespace
  fromMaybe anyMatcher <$> (optionMaybe $ choice [regexmatcher, try negativeregexmatcher, numericmatcher, negativenumericmatcher])
 ) <?> "expected exit status"

-- The format 3 lines matcher consumes lines until one of these:
-- 1. another section delimiter in this test (>, >2, >=)
-- 2. the next test's start delimiter (<, $), or the start of blank/comment lines preceding it
-- 3. end of file, or the start of blank/comment lines preceding it
linesmatcher3 :: Parser Matcher
linesmatcher3 = do
  ptrace_ "    linesmatcher3 0"
  ln <- sourceLine <$> getPosition
  ls <- unlines <$>
        line `manyTill` lookAhead (choice' [delimiterNotNewTest3
                                           ,many whitespaceorcommentline >> delimiterNewTest3
                                           ,many whitespaceorcommentline >> eofasstr])
        <?> "lines of output"
  ptrace "    linesmatcher3 ." ls
  return $ Lines ln ls

delimiterNewTest3 = do
  -- ptrace_ "     delimiterNewTest 0"
  choice [string "$", string "<"]

delimiterNotNewTest3 = do
  -- ptrace_ "    delimiterNotNewTest 0"
  choice [try (string ">2"), try (string ">="), string ">"]

----------------------------------------------------------------------
-- common

linesBetween :: [String] -> [String] -> Parser String
linesBetween startdelims enddelims = do
  let delimp "" = string ""
      delimp s  = string s <* whitespace <* newline
  Utils.choice' $ map delimp startdelims
  let end = choice $ (map (try . string) enddelims) ++ [eofasstr]
  unlines <$> line `manyTill` lookAhead end

fixedcommand,replaceablecommand :: Parser TestCommand
fixedcommand = many1 whitespacechar >> line >>= return . FixedCommand
replaceablecommand = line >>= return . ReplaceableCommand

regexmatcher :: Parser Matcher
regexmatcher = (do
  char '/'
  r <- (try escapedslash <|> noneOf "/") `manyTill` (char '/')
  whitespaceorcommentlineoreof
  return $ PositiveRegex r) <?> "regex pattern"

escapedslash :: Parser Char
escapedslash = char '\\' >> char '/'

negativeregexmatcher :: Parser Matcher
negativeregexmatcher = (do
  char '!'
  PositiveRegex r <- regexmatcher
  return $ NegativeRegex r) <?> "negative regex pattern"

numericmatcher :: Parser Matcher
numericmatcher = (do
  s <- many1 $ oneOf "0123456789"
  whitespaceorcommentlineoreof
  return $ Numeric s
  ) <?> "number match"

negativenumericmatcher :: Parser Matcher
negativenumericmatcher = (do
  char '!'
  Numeric s <- numericmatcher
  return $ NegativeNumeric s
  ) <?> "negative number match"

linesmatcher1 :: Parser Matcher
linesmatcher1 = do
  ln <- sourceLine <$> getPosition
  (Lines ln . unlines <$>) (line `manyTill` (lookAhead delimiter)) <?> "lines of output"

delimiter = choice [string "$$$", string "<<<", try (string ">>>2"), try (string ">>>="), string ">>>", eofasstr]

-- longdelims = ["<<<", "$$$", ">>>2", ">>>=", ">>>"]
-- shortdelims = ["<", "$", ">2", ">=", ">"]

-- newlineoreof, whitespacechar :: Parser Char
-- line,lineoreof,whitespace,whitespaceline,commentline,whitespaceorcommentline,whitespaceorcommentlineoreof,delimiter,input :: Parser String

-- lineoreof = (anyChar `manyTill` newlineoreof)
-- XXX shouldn't it be:
lineoreof = (noneOf "\n" `manyTill` newlineoreof)
newlineoreof = newline <|> (eof >> return '\n') <?> "newline or end of file"
line = (anyChar `manyTill` newline) <?> "rest of line"
whitespacechar = oneOf " \t"
whitespace = many whitespacechar
whitespaceline = try (newline >> return "") <|> try (whitespacechar >> whitespacechar `manyTill` newlineoreof)
-- a line beginning with optional whitespace and #, or beginning with one or more * (an org node)
commentline = try ((many1 (char '*') <|> (whitespace >> many1 (char '#'))) >> lineoreof) <?> "comments"
whitespaceorcommentline = commentline <|> whitespaceline
whitespaceorcommentlineoreof = choice [eofasstr, commentline, whitespaceline]
eofasstr = eof >> return ""
