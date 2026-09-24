module Expression(Expr(..), expression) where
import Lexer
import Text.Parsec.String (Parser)
import Text.Parsec (unexpected, (<|>), try)
import Text.Printf (printf)

data Expr =
    ExprIdent String |
    ExprNum Number |
    ExprString String |
    ExprChar Char

unexpectedGot :: String -> Token -> Parser a
unexpectedGot msg t = unexpected $ printf "%s, got %s" msg (show t)

tokenTryMap :: (Token -> Maybe a) -> String -> Parser a
tokenTryMap func errMsg = do
    t <- token
    case func t of
        Just a -> return a
        Nothing -> unexpectedGot errMsg t

tokenNum :: Parser Number
tokenNum = tokenTryMap func "expected number"
    where
        func (TokenNum n) = Just n
        func _ = Nothing

tokenString :: Parser String
tokenString = tokenTryMap func "expected string literal"
    where
        func (TokenString s) = Just s
        func _ = Nothing

tokenChar :: Parser Char
tokenChar = tokenTryMap func "expected char literal"
    where
        func (TokenChar ch) = Just ch
        func _ = Nothing

tokenIdent :: Parser String
tokenIdent = tokenTryMap func "expected identifier"
    where
        func (TokenIdent i) = Just i
        func _ = Nothing

primary :: Parser Expr
primary =
    (ExprNum <$> (try tokenNum)) <|>
    (ExprChar <$> (try tokenChar)) <|>
    (ExprString <$> (try tokenString)) <|>
    (ExprIdent <$> (try tokenIdent))

expression :: Parser Expr
expression = primary
