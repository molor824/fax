{-# LANGUAGE LambdaCase #-}

module Expression(Expr(..), expression) where
import Lexer
import Text.Parsec.String (Parser)
import Text.Parsec (unexpected, (<|>), try, sepEndBy)
import Text.Printf (printf)

data Expr =
    ExprIdent String |
    ExprNum Number |
    ExprString String |
    ExprChar Char |
    ExprArray [Expr] |
    ExprTuple [Expr] |
    ExprCall (Expr, [Expr]) |
    ExprProperty (Expr, String)
    deriving Show

unexpectedGot :: String -> Token -> Parser a
unexpectedGot msg t = unexpected $ printf "%s, got %s" msg (show t)

tokenNum :: Parser Number
tokenNum = token >>= \case
    TokenNum n -> return n
    t -> unexpectedGot "expected number" t

tokenString :: Parser String
tokenString = token >>= \case
    TokenString s -> return s
    t -> unexpectedGot "expected string literal" t

tokenChar :: Parser Char
tokenChar = token >>= \case
    TokenChar ch -> return ch
    t -> unexpectedGot "expected char literal" t

tokenIdent :: Parser String
tokenIdent = token >>= \case
    TokenIdent i -> return i
    t -> unexpectedGot "expected identifier" t

tokenKeyword :: Keyword -> Parser Keyword
tokenKeyword kwd = token >>= \case
    TokenKeyword kwd1 | kwd == kwd1 -> return kwd
    t -> unexpectedGot (printf "expected keyword %s" $ show kwd) t

tokenSymbol :: Symbol -> Parser Symbol
tokenSymbol sym = token >>= \case
    TokenSymbol sym1 | sym == sym1 -> return sym
    t -> unexpectedGot (printf "expected symbol %s" $ show sym) t

args :: Parser [Expr]
args = expression `sepEndBy` (try $ tokenSymbol SymbolComma)

array :: Parser Expr
array = do
    _ <- try $ tokenSymbol SymbolLSquare
    elems <- args
    _ <- tokenSymbol SymbolRSquare
    return $ ExprArray elems

tuple :: Parser Expr
tuple = do
    _ <- try $ tokenSymbol SymbolLCurly
    elems <- args
    _ <- tokenSymbol SymbolRCurly
    return $ ExprTuple elems

group :: Parser Expr
group = do
    _ <- try $ tokenSymbol SymbolLParen
    expr <- expression
    _ <- tokenSymbol SymbolRParen
    return expr

primary :: Parser Expr
primary =
    array <|>
    tuple <|>
    group <|>
    (ExprNum <$> (try tokenNum)) <|>
    (ExprChar <$> (try tokenChar)) <|>
    (ExprString <$> (try tokenString)) <|>
    (ExprIdent <$> (try tokenIdent))

postfix :: Parser Expr
postfix = do
    operand <- primary
    call operand <|>
        property operand <|>
        return operand

call :: Expr -> Parser Expr
call operand = do
    _ <- try $ tokenSymbol SymbolLParen
    elems <- args
    _ <- tokenSymbol SymbolRParen
    return $ ExprCall (operand, elems)

property :: Expr -> Parser Expr
property operand = do
    _ <- try $ tokenSymbol SymbolDot
    name <- tokenIdent
    return $ ExprProperty (operand, name)

expression :: Parser Expr
expression = postfix
