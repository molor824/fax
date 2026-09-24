{-# LANGUAGE LambdaCase #-}

module Lexer(
    token,
    Token(..),
    Number(..)
) where

import Text.Printf (printf)
import Data.Char (isAlphaNum, isDigit, isHexDigit, digitToInt, isAlpha, isSpace, chr)
import Text.Parsec (Parsec, unexpected, (<|>), many1, try, many, manyTill, string, satisfy, char, oneOf, anyChar, string')
import Control.Applicative (Alternative(empty))

type Parser = Parsec String ()

radixDigit :: Int -> Parser Int
radixDigit radix = do
    c <- if radix > 10 then satisfy isAlphaNum else satisfy isDigit
    if not (isHexDigit c)
        then unexpected $ printf "character '%c' is not a valid digit." c
    else let d = digitToInt c in if d > radix
        then unexpected $ printf "digit '%c' is not in the base %d range." c radix
    else return d

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

instance Show Number where
    show (NumWhole int) = show int
    show (NumReal real) = show real

numLiteral :: Parser Number
numLiteral = NumWhole <$> (radixSelector >>= radixInteger) <|> (NumReal <$> fullDouble)

identifier :: Parser String
identifier = do
    first <- satisfy $ \c -> isAlpha c || c == '_'
    second <- many $ satisfy $ \c -> isAlphaNum c || c == '_'
    return (first : second)

skipSpace :: Parser ()
skipSpace = const () <$> (satisfy $ \c -> isSpace c && c /= '\n')

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
skip = const () <$> (many (skipSpace <|> skipComment))

hexCode :: Int -> Parser Int
hexCode 1 = radixDigit 16
hexCode n = do
    num <- hexCode (n - 1)
    d <- radixDigit 16
    return $ num * 16 + d

stringLiteral :: Parser String
stringLiteral = do
    _ <- char '"'
    manyTill (escapeChar <|> anyChar) $ char '"'

charLiteral :: Parser Char
charLiteral = do
    _ <- char '\''
    ch <- escapeChar <|> (
        anyChar >>= \case
            '\'' -> unexpected "character literal cannot be empty. note: if your intention was to type the literal single quote, then use '\\''"
            c -> return c
        ) <|> unexpected "expected character"
    _ <- char '\'' <|> unexpected "expected character literal terminator"
    return ch

escapeChar :: Parser Char
escapeChar = do
    _ <- char '\\'
    escape <- oneOf "0abfnrtvxuU\\'\""
    case escape of
        '0' -> return '\0'
        'a' -> return '\a'
        'b' -> return '\b'
        'f' -> return '\f'
        'n' -> return '\n'
        'r' -> return '\r'
        't' -> return '\t'
        'v' -> return '\v'
        '\\' -> return '\\'
        '\'' -> return '\''
        '"' -> return '"'
        'x' -> do
            code <- hexCode 2 <|> unexpected "expected hex code after '\\x'. must satisfy \\xhh (where h is hexadecimal digit)"
            if code >= 0x80
                then unexpected $ printf "escape code %02X is invalid for ASCII. must satisfy (0x0 <= x < 0x80)" code
                else return $ chr code
        c | c == 'U' || c == 'u' ->
            (chr <$> hexCode (if c == 'u' then 4 else 8)) <|>
            (unexpected $ printf "expected hex code after '\\%c'. must satisfy \\uhhhh or \\Uhhhhhhhh (where h is hexadecimal digit)" c)
        c -> error $ printf "escape %c should be unreachable" c

data Symbol =
    SymbolAdd |
    SymbolSub |
    SymbolMul |
    SymbolDiv |
    SymbolBitOr |
    SymbolBitAnd |
    SymbolPower |
    SymbolNot |
    SymbolBitNot |
    SymbolLParen |
    SymbolRParen |
    SymbolRSquare |
    SymbolLSquare |
    SymbolRCurly |
    SymbolLCurly |
    SymbolShl |
    SymbolShr |
    SymbolSha |
    SymbolLt |
    SymbolGt |
    SymbolLe |
    SymbolGe |
    SymbolEq |
    SymbolNe |
    SymbolAssign |
    SymbolArrow |
    SymbolSemicolon |
    SymbolColon |
    SymbolComma |
    SymbolDot
    deriving Eq

symParseTable :: [(String, Symbol)]
symParseTable =
    [
        ("+", SymbolAdd),
        ("->", SymbolArrow),
        ("-", SymbolSub),
        ("*", SymbolMul),
        ("/", SymbolDiv),
        ("|", SymbolBitOr),
        ("&", SymbolBitAnd),
        ("^", SymbolPower),
        ("!", SymbolNot),
        ("~", SymbolBitNot),
        ("(", SymbolLParen),
        (")", SymbolRParen),
        ("[", SymbolLSquare),
        ("]", SymbolRSquare),
        ("{", SymbolLCurly),
        ("}", SymbolRCurly),
        ("<<", SymbolShl),
        (">>", SymbolShr),
        (">>>", SymbolSha),
        ("<=", SymbolLe),
        (">=", SymbolGe),
        ("<", SymbolLt),
        (">", SymbolGt),
        ("==", SymbolEq),
        ("!=", SymbolNe),
        ("=", SymbolAssign),
        (";", SymbolSemicolon),
        (":", SymbolColon),
        (",", SymbolComma),
        (".", SymbolDot)
    ]

symbol' :: [(String, Symbol)] -> Parser Symbol
symbol' ((str, sym):rest) = (const sym <$> string' str) <|> symbol' rest
symbol' [] = empty

symbol :: Parser Symbol
symbol = symbol' symParseTable

showSymbol' :: [(String, Symbol)] -> Symbol -> String
showSymbol' [] _ = error "should be unreachable. symParseTable might be empty!"
showSymbol' ((str, sym):rest) sym'
    | sym == sym' = "'" ++ str ++ "'"
    | otherwise = showSymbol' rest sym'

instance Show Symbol where
    show = showSymbol' symParseTable

data Keyword =
    KeywordAnd |
    KeywordOr |
    KeywordAs |
    KeywordIf |
    KeywordElse |
    KeywordLoop |
    KeywordDo
    deriving Eq

kwdParseTable :: [(String, Keyword)]
kwdParseTable =
    [
        ("and", KeywordAnd),
        ("or", KeywordOr),
        ("as", KeywordAs),
        ("if", KeywordIf),
        ("else", KeywordElse),
        ("loop", KeywordLoop),
        ("do", KeywordDo)
    ]

asKeyword' :: [(String, Keyword)] -> String -> Maybe Keyword
asKeyword' [] _ = Nothing
asKeyword' ((str, kwd):rest) str'
    | str == str' = Just kwd
    | otherwise = asKeyword' rest str'

asKeyword :: String -> Maybe Keyword
asKeyword = asKeyword' kwdParseTable

showKeyword' :: [(String, Keyword)] -> Keyword -> String
showKeyword' [] _ = error "should be unreachable. kwdParseTable might be incomplete!"
showKeyword' ((str, kwd):rest) kwd'
    | kwd == kwd' = str
    | otherwise = showKeyword' rest kwd'

instance Show Keyword where
    show = showKeyword' kwdParseTable

data Token =
    TokenNum Number |
    TokenKeyword Keyword |
    TokenIdent String |
    TokenChar Char |
    TokenString String |
    TokenSymbol Symbol

token :: Parser Token
token = skip >> (
    (TokenSymbol <$> symbol) <|>
    (TokenString <$> stringLiteral) <|>
    (TokenChar <$> charLiteral) <|>
    (TokenNum <$> numLiteral) <|>
    do
        ident <- identifier
        return $ case asKeyword ident of
            Just kwd -> TokenKeyword kwd
            Nothing -> TokenIdent ident
    )

instance Show Token where
    show (TokenNum n) = show n
    show (TokenKeyword kwd) = show kwd
    show (TokenIdent str) = str
    show (TokenString str) = show str
    show (TokenChar ch) = show ch
    show (TokenSymbol sym) = show sym
