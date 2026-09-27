module Main (main) where

import Expression
import Text.Parsec

main :: IO ()
main = do
    input <- getContents
    case parse (many1 expression) "" input of
        Right ts -> printExpr ts
        Left err -> print err

printExpr :: [Expr] -> IO ()
printExpr [] = return ()
printExpr (t:rest) = print t >> printExpr rest
