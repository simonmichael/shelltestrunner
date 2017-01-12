module Preprocessor
  ( preprocess,
    createMacroPreProcessor,
    getMacros
  )
  where

import Text.Parsec  
import Data.List
import Types


{- Apply preprocessor functions in sequence to change a string. -}
preprocess :: PreProcessor -> String -> Either ParseError String
preprocess NoPreprocess [] = Right []
preprocess NoPreprocess str = Right str
preprocess (PreProcessor flist) str = foldl (\ac f -> case ac of Right a -> f a
                                                                 err -> err) (Right str) flist
{- Creates a macro [(macro,value)] preprocessor that replaces all substrings with macro values. -}
createMacroPreProcessor :: [Macro] -> PreProcessor
createMacroPreProcessor [] = NoPreprocess
createMacroPreProcessor macro = PreProcessor [\s -> Right (replaceMacros macro s)]
                                                                 
{- substring -> replacement -> input -}
replace :: String -> String -> String -> String
replace _ _ [] = []
replace [] _ str = str
replace _ [] str = str
replace sub rep str = let compareLen = length sub
                          subtoken = take compareLen str
                          afterSubtoken = drop compareLen str
                          (s:xs) = str
                      in case sub == subtoken of False -> s:(replace sub rep xs)
                                                 True -> rep ++(replace sub rep afterSubtoken)

{- macros -> replacement -> input -}
replaceMacros :: [Macro] -> String -> String
replaceMacros [] str = str
replaceMacros macro str = Prelude.foldl (\ac m -> let 
                                                      (key, replacment) = m
                                                  in replace key replacment ac) str macro
{- Read macro from string <macro>=<value>. -}            
readMacro :: String -> Maybe Macro
readMacro str = do
        i <- elemIndex '=' str
        return (take i str, drop (i + 1) str)

{- Get macros from input parameters. -}
getMacros :: [String] -> [Macro]
getMacros str = let m = map readMacro str
                    tmp = filter (\a -> case a of Nothing -> False
                                                  _ -> True) m
                in map (\a -> let Just x = a in x) tmp
