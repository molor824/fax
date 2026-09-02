module Main (main) where

import Lexer
import Text.Parsec

main :: IO ()
main = do
    input <- readFile ".test.fax"
    let result = parse (many tokenParser) "" input
    case result of
        Right ts -> printTokens ts
        Left err -> print err

printTokens :: [Token] -> IO ()
printTokens [] = return ()
printTokens (t:rest) = print t >> printTokens rest
