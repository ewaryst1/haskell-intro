import Text.Parsec
import Text.Parsec.String
import Control.Applicative ((<$>))

data Expr = Mult Expr Expr | Val Double
    deriving (Show, Read)


aNumber :: Parser Double
aNumber = rd <$> many1 digit
    where
        rd = read :: String -> Double

exprParser :: Parser Expr
exprParser = withParens <|> mult <|> leaf
  where
    leaf = Val <$> aNumber
    withParens = (char '(') >> exprParser <* (char ')')
    mult = do
       l <- exprParser
       char '*'
       r <- exprParser
       return $ Mult l r


simpleExprParser :: Parser Expr
simpleExprParser = do
    char '('
    spaces
    l <- aNumber
    spaces
    char '*'
    spaces
    r <- aNumber
    spaces
    char ')'
    return $ Mult (Val l) (Val r)