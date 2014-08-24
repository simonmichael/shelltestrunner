module Parse
where

import Text.ParserCombinators.Parsec

import Import
import Types
import qualified Utils


-- Enable this to see parser debug output.
dbg = False
ptrace_ s  | dbg       = Utils.ptrace s
           | otherwise = return ()
ptrace s a | dbg       = Utils.ptrace $ s ++ ": " ++ show a
           | otherwise = return ()

-- | Try to parse this shelltest file and return the number of tests
-- parsed, or 0 if there was a parse error.
testparse :: FilePath -> IO Int
testparse f = parseFromFile shelltestfile f  >>= return . either (const 0) length

-- | Parse this shell test file, optionally logging debug output.
parseShellTestFile :: Bool -> FilePath -> IO (Either ParseError [ShellTest])
parseShellTestFile debug f = do
  p <- parseFromFile shelltestfile f
  case p of
    Right ts -> do
           let ts' | length ts > 1 = [t{testname=testname t++":"++show n} | (n,t) <- zip ([1..]::[Int]) ts]
                   | otherwise     = ts
           when (debug) $ do
             printf "parsed %s:\n" f
             mapM_ (putStrLn.(' ':).show) ts'
           return $ Right ts'
    Left _ -> do
           when (debug) $ printf "failed to parse any tests in %s\n" f
           return p

-- parsers

shelltestfile :: Parser [ShellTest]
shelltestfile = do
  ptrace_ "shelltestfile 0"
  ts <- concat <$> many (try format2testgroup <|> ((:[]) <$> try format1test))
  ptrace "shelltestfile ts" ts
  skipMany whitespaceorcommentline
  ptrace_ "shelltestfile 2"
  eof
  ptrace_ "shelltestfile ."
  return ts

-- format 1 (v1.0+) - each test specifies its input; missing test parts are not checked

format1test :: Parser ShellTest
format1test = do
  ptrace_ " format1test 0"
  skipMany whitespaceorcommentline
  ptrace_ " format1test 1"
  c <- commandp <?> "command line"
  ptrace " format1test c" c
  i <- optionMaybe input <?> "input"
  ptrace " format1test i" i
  o <- optionMaybe expectedoutput1 <?> "expected output"
  ptrace " format1test o" o
  e <- optionMaybe expectederror <?> "expected error output"
  ptrace " format1test e" e
  x <- expectedexitcode <?> "expected exit status"
  ptrace " format1test x" x
  when (null (show c) && (isNothing i) && (null $ catMaybes [o,e]) && null (show x)) $ fail ""
  f <- sourceName . statePos <$> getParserState
  let t = ShellTest{testname=f,command=c,stdin=i,stdoutExpected=o,stderrExpected=e,exitCodeExpected=x}
  ptrace " format1test ." t
  return t

-- format 2 (v1.4+) - input first, then tests; missing test parts are implicit checks

format2testgroup :: Parser [ShellTest]
format2testgroup = do
  ptrace_ " format2testgroup 0"
  skipMany whitespaceorcommentline
  ptrace_ " format2testgroup 1"
  i <- optionMaybe (linesBetween ["<<<",""] ["$$$","<<<"]) <?> "input"
  ptrace " format2testgroup i" i
  ts <- many1 $ try (format2test i)
  ptrace " format2testgroup ." ts
  return ts

format2test :: Maybe String -> Parser ShellTest
format2test i = do
  ptrace_ "  format2test 0"
  skipMany whitespaceorcommentline
  ptrace_ "  format2test 1"
  c <- commandp' <?> "command line"
  ptrace "  format2test c" c
  nullstdout <- nullLinesMatcher . sourceLine <$> getPosition
  o <- maybe (Just nullstdout) Just <$> optionMaybe expectedoutput2 <?> "expected output"
  ptrace "  format2test o" o
  nullstderr <- nullLinesMatcher . sourceLine <$> getPosition
  e <- maybe (Just $ nullstderr) Just <$> optionMaybe expectederror <?> "expected error output"
  ptrace "  format2test e" e
  x <- fromMaybe nullStatusMatcher <$> optionMaybe expectedexitcode <?> "expected exit status"
  ptrace "  format2test x" x
  when (null (show c) && (isNothing i) && (null $ catMaybes [o,e]) && null (show x)) $ fail ""
  f <- sourceName . statePos <$> getParserState
  let t = ShellTest{testname=f,command=c,stdin=i,stdoutExpected=o,stderrExpected=e,exitCodeExpected=x}
  ptrace "  format2test ." t
  return t

--

nullLinesMatcher n = Lines n ""
nullStatusMatcher  = Numeric "0"

input = try $ string "<<<" >> whitespaceorcommentline >> unlines <$> (line `manyTill` (lookAhead (try delimiter)))

commandp,fixedcommand,replaceablecommand :: Parser TestCommand
commandp = optional (string "$$$") >> (fixedcommand <|> replaceablecommand)
commandp' = string "$$$" >> (fixedcommand <|> replaceablecommand)
fixedcommand = many1 whitespacechar >> line >>= return . FixedCommand
replaceablecommand = line >>= return . ReplaceableCommand

expectedoutput1 :: Parser Matcher
expectedoutput1 = (try $ do
  string ">>>"
  whitespace
  choice [positiveregexmatcher, negativeregexmatcher, whitespaceorcommentlineoreof >> linesmatcher]
 ) <?> "expected output"

-- In format 2, >>> is used only with /REGEX/, also don't consume the
-- whitespace/comments immediately preceding a following test.
expectedoutput2 :: Parser Matcher
expectedoutput2 = (try $ do
  (try $ do string ">>>"
            whitespace
            (positiveregexmatcher <|> negativeregexmatcher)
              <|> (do p <- getPosition
                      error $ ">>> should be used only with a following /REGEX/ at "++show p))
  <|> linesmatcher2
 ) <?> "expected output"

expectederror :: Parser Matcher
expectederror = (try $ do
  string ">>>2"
  whitespace
  choice [positiveregexmatcher, negativeregexmatcher, (whitespaceorcommentlineoreof >> linesmatcher)]
 ) <?> "expected error output"

expectedexitcode :: Parser Matcher
expectedexitcode = (try $ do
  string ">>>="
  whitespace
  choice [positiveregexmatcher, try negativeregexmatcher, numericmatcher, negativenumericmatcher]
 ) <?> "expected exit status"

linesmatcher :: Parser Matcher
linesmatcher = do
  ln <- sourceLine <$> getPosition
  (Lines ln . unlines <$>) (line `manyTill` (lookAhead delimiter)) <?> "lines of output"

-- For format 2, don't consume the whitespace/comments immediately preceding a following test.
linesmatcher2 :: Parser Matcher
linesmatcher2 = do
  -- ptrace_ "   linesmatcher2 0"
  ln <- sourceLine <$> getPosition
  let endmarker = delimiterNotNewTest <|> try newTest <|> eofasstr
  (Lines ln . unlines) <$> (line `manyTill` (lookAhead endmarker)) <?> "lines of output"


negativeregexmatcher :: Parser Matcher
negativeregexmatcher = (do
  char '!'
  PositiveRegex r <- positiveregexmatcher
  return $ NegativeRegex r) <?> "non-matched regex pattern"

positiveregexmatcher :: Parser Matcher
positiveregexmatcher = (do
  char '/'
  r <- (try escapedslash <|> noneOf "/") `manyTill` (char '/')
  whitespaceorcommentlineoreof
  return $ PositiveRegex r) <?> "regex pattern"

negativenumericmatcher :: Parser Matcher
negativenumericmatcher = (do
  char '!'
  Numeric s <- numericmatcher
  return $ NegativeNumeric s
  ) <?> "non-matched number"

numericmatcher :: Parser Matcher
numericmatcher = (do
  s <- many1 $ oneOf "0123456789"
  whitespaceorcommentlineoreof
  return $ Numeric s
  ) <?> "number"

escapedslash :: Parser Char
escapedslash = char '\\' >> char '/'

linesBetween :: [String] -> [String] -> Parser String
linesBetween startdelims enddelims = do
  let delimp "" = string ""
      delimp s  = string s <* whitespace <* newline
  Utils.choice' $ map delimp startdelims
  let end = choice $ (map (try . string) enddelims) ++ [eofasstr]
  unlines <$> line `manyTill` lookAhead end

delimiter = choice [string "$$$", string "<<<", try (string ">>>2"), try (string ">>>="), string ">>>", eofasstr]

newTest = do
  -- ptrace_ "    newTest 0"
  many whitespaceorcommentline
  delimiterNewTest

delimiterNewTest = do
  -- ptrace_ "     delimiterNewTest 0"
  choice [string "$$$", string "<<<"]

delimiterNotNewTest = do
  -- ptrace_ "    delimiterNotNewTest 0"
  choice [try (string ">>>2"), try (string ">>>="), string ">>>"]

-- longdelims = ["<<<", "$$$", ">>>2", ">>>=", ">>>"]
-- shortdelims = ["<", "$", ">2", ">=", ">"]

newlineoreof, whitespacechar :: Parser Char
line,lineoreof,whitespace,whitespaceline,commentline,whitespaceorcommentline,whitespaceorcommentlineoreof,delimiter,input :: Parser String
line = (anyChar `manyTill` newline) <?> "rest of line"
newlineoreof = newline <|> (eof >> return '\n') <?> "newline or end of file"
lineoreof = (anyChar `manyTill` newlineoreof)
whitespacechar = oneOf " \t"
whitespace = many whitespacechar
whitespaceline = try (newline >> return "") <|> try (whitespacechar >> whitespacechar `manyTill` newlineoreof)
commentline = try (whitespace >> char '#' >> lineoreof) <?> "comments"
whitespaceorcommentline = commentline <|> whitespaceline
whitespaceorcommentlineoreof = choice [eofasstr, commentline, whitespaceline]

eofasstr = eof >> return ""
