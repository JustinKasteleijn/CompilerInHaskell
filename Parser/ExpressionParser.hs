
import Parser 
import AST
import Control.Applicative (Alternative(..))

parseStatements :: Parser [Statement]
parseStatements = parseStatement `sepBy1` (whitespace *> string "\\n" <* whitespace)

parseStatement :: Parser Statement 
parseStatement = parseIfStatement

parseIfStatement :: Parser Statement
parseIfStatement = do 
  _     <- string "if"
  _     <- whitespace *> char '('
  cond  <- whitespace *> parseExpression
  _     <- whitespace *> char ')'
  _     <- whitespace *> string "then"
  expr  <- whitespace *> parseExpression
  _     <- whitespace *> string "else"
  expr' <- whitespace *> parseExpression
  return $ (If cond expr expr') 

parseExpression :: Parser Expr
parseExpression = parseCondition
  <|> parseArith
  
parseCondition :: Parser Expr 
parseCondition = do 
  x <- parseArith
  _ <- token $ string "=="
  y <- parseArith 
  return $ x :==: y 

-- Study more feels a lil bit like magic 
parseArith :: Parser Expr
parseArith = do
  x <- parseLiteral
  rest x
  where
    rest x = 
      (do _ <- token $ char '+'
          y <- parseLiteral
          rest (x :+: y)) <|>
      (do _ <- token $ char '-'
          y <- parseLiteral
          rest (x :-: y)) <|>
      pure x

parseLiteral :: Parser Expr 
parseLiteral = (Literal <$> parseVal)

parseVal :: Parser Val 
parseVal = (ValBool True <$ string "true") 
  <|> (ValBool False <$ string "false")
  <|> (ValInt <$> int)

main :: IO ()
main = do 
  expr <- getLine
  case parse parseStatements expr of 
    Just (stmts, _) -> execute stmts
    _               -> print $ "invalid input"
  main 

