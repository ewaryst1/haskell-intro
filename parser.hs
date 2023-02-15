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
    noMult = withParens <|> leaf
    mult = do
       l <- noMult
       char '*'
       r <- exprParser
       return $ Mult l r

exprParser2 :: Parser Expr
exprParser2 = do
     l <- noMult
     option l $ multRemainder l
  where
    leaf = Val <$> aNumber
    withParens = (char '(') >> exprParser2 <* (char ')')
    noMult = withParens <|> leaf
    multRemainder l = do
       char '*'
       r <- exprParser2
       return $ Mult l r

ep = exprParser2 <* eof

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