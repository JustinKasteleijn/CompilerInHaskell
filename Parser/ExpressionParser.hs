import Parser 
import AST
import Control.Applicative (Alternative(..))

parseStatements :: Parser [Statement]
parseStatements = parseStatement `sepBy1` (token $ string "\\n")

parseStatement :: Parser Statement
parseStatement = parseIfStatement 
  <|> parseAssignment
  <|> parsePrint

parseIfStatement :: Parser Statement
parseIfStatement = do 
  _     <- string "if"
  _     <- whitespace *> char '('
  cond  <- whitespace *> parseExpression
  _     <- whitespace *> char ')'
  _     <- whitespace *> string "then"
  stmt  <- whitespace *> parseStatements
  _     <- whitespace *> string "else"
  stmt' <- whitespace *> parseStatements
  return $ If cond stmt stmt'

parseAssignment :: Parser Statement
parseAssignment = do
  var <- token $ some alphaNum 
  _   <- token $ char '='
  expr <- parseExpression
  return $ Assignment var expr
  
parsePrint :: Parser Statement 
parsePrint = Print 
  <$  string "print" 
  <*  whitespace
  <*> parseExpression

parseExpression :: Parser Expr
parseExpression = parseCondition
  <|> parseArith
  <|> parseVar
  <|> parseLiteral

parseCondition :: Parser Expr 
parseCondition = do 
  x <- whitespace *> parseLiteral <|> parseVar
  _ <- token $ string "=="
  y <- parseExpression
  return $ x :==: y 
                                                                                                                                         
parseArith :: Parser Expr
parseArith = do
  x <- parseLiteral <|> parseVar
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
      
parseVar :: Parser Expr 
parseVar = Var <$> (some (alphaNum))

parseLiteral :: Parser Expr 
parseLiteral = (Literal <$> parseVal)

parseVal :: Parser Val 
parseVal = (ValBool True <$ string "true") 
  <|> (ValBool False <$ string "false")
  <|> (ValInt <$> int)

main :: IO ()
main = do 
  expr <- getLine
  print $ parse parseStatements expr
  case parse parseStatements expr of 
    Just (stmts, _) -> execute stmts []
    _               -> print $ "invalid input"
  main 
