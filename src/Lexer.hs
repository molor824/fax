module Lexer(
    tokenParser,
    Token(..)
) where

import Text.Printf
import Text.Parsec.String(Parser)
import Data.Char
import Text.Parsec

data Spanned a = Spanned {
    spanStart :: SourcePos,
    spanEnd :: SourcePos,
    spanVal :: a} deriving Show

spanned :: Parser a -> Parser (Spanned a)
spanned p = do
    start <- getPosition
    x <- p
    end <- getPosition
    return $ Spanned start end x

radixDigit :: Int -> Parser Int
radixDigit radix = do
    c <- if radix > 10 then satisfy isAlphaNum else satisfy isDigit
    if not (isHexDigit c)
        then unexpected $ printf "character '%c' is not a valid digit." c
    else let d = digitToInt c in if d > radix
        then unexpected $ printf "digit '%c' is not in the base %d range." c radix
    else
        return d

radixInteger :: Int -> Parser Integer
radixInteger radix = do
    digits <- many1 $ radixDigit radix
    return $ foldl digitAccum 0 digits
    where digitAccum i d = i * (fromIntegral radix) + (fromIntegral d)

radixSelector :: Parser Int
radixSelector =
    (return 16 <$> string' "0x") <|>
    (return 8 <$> string' "0o") <|>
    (return 2 <$> string' "0b") <|>
    return 10

fractionDouble :: Bool -> Parser Double
fractionDouble alone = do
    _ <- char '.'
    digits <- (if alone then many1 else many) $ radixDigit 10
    return $ foldl (\v d -> v * 10.0 + fromIntegral d) 0.0 digits

pointedDouble :: Parser Double
pointedDouble = (do
    whole <- radixInteger 10
    fractional <- fractionDouble False
    return $ fromIntegral whole + fractional
    ) <|> fractionDouble True

fullDouble :: Parser Double
fullDouble = do
    value <- pointedDouble
    (do
        _ <- oneOf "eE"
        sign <- oneOf "+-" <|> return '+'
        exp' <- radixInteger 10
        return $
            last $
            takeWhile (\v -> v /= 0.0 && not (isInfinite v || isNaN v)) $
            take (fromIntegral exp') $ iterate (* if sign == '+' then 10.0 else 0.1) value
        ) <|> return value

data Number = NumWhole Integer | NumReal Double
    deriving Show

numLiteral :: Parser Number
numLiteral = NumWhole <$> (radixSelector >>= radixInteger) <|> (NumReal <$> fullDouble)

identifier :: Parser String
identifier = do
    first <- satisfy $ \c -> isAlpha c || c == '_'
    second <- many $ satisfy $ \c -> isAlphaNum c || c == '_'
    return $ [first] ++ second

skipSpace :: Parser ()
skipSpace = const () <$> satisfy isSpace

skipLineComment :: Parser ()
skipLineComment = do
    _ <- string' "--"
    _ <- manyTill anyChar $ char '\n'
    return ()

skipComment :: Parser ()
skipComment = (do
    depth <- try $ do
        _ <- string "--"
        length <$> (many1 $ char '[')
    _ <- manyTill anyChar $ string' $ take depth $ repeat ']'
    return ()) <|> skipLineComment

skip :: Parser ()
skip = const () <$> (many $ (skipSpace <|> skipComment))

data Token = TokenNum Number | TokenIdent String
    deriving Show

tokenParser :: Parser Token
tokenParser = do
    skip
    (TokenIdent <$> identifier) <|>
        (TokenNum <$> numLiteral)
