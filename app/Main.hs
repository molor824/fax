module Main (main) where

import Lexer
import Text.Parsec

main :: IO ()
main = do
    input <- getContents
    print $ parse tokenParser "" input
