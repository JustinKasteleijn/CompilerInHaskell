import Parser 
import AST
import Control.Applicative (Alternative(..))

parseStatements :: Parser [Statement]
parseStatements = parseStatement `sepBy1` lineBreak

parseStatement :: Parser Statement
parseStatement = parseIfStatement 
  <|> parseAssignment
  <|> parseWhile
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
  
parseWhile :: Parser Statement
parseWhile = While 
  <$  string "while"  
  <*> (whitespace *> char '(' *> parseExpression <* whitespace <* char ')')
  <*> (char '\n' *> indentedStatements)
  
parsePrint :: Parser Statement 
parsePrint = Print 
  <$  string "print" 
  <*  whitespace
  <*> parseExpression
  
indentedStatement :: Parser Statement
indentedStatement = indented *> parseStatement

indentedStatements :: Parser [Statement]
indentedStatements = indentedStatement `sepBy1` (char '\n')

parseExpression :: Parser Expr
parseExpression = parseCondition
  <|> parseArith
  <|> parseVar
  <|> parseLiteral

parseCondition :: Parser Expr 
parseCondition = parseEquals 
  <|> parseNotEquals
  <|> parseGT
  <|> parseLT

parseEquals :: Parser Expr 
parseEquals = do 
  x <- whitespace *> parseLiteral <|> parseVar
  _ <- token $ string "=="
  y <- parseExpression
  return $ x :==: y 
  
parseNotEquals :: Parser Expr
parseNotEquals = (:!=:) 
  <$> (whitespace *> parseLiteral <|> parseVar)
  <*  (token (string "!="))
  <*> parseExpression
  
parseGT :: Parser Expr 
parseGT = (:>:) 
  <$> (whitespace *> parseLiteral <|> parseVar)
  <*  (token (char '>'))
  <*> parseExpression
  
parseLT :: Parser Expr 
parseLT = (:<:) 
  <$> (whitespace *> parseLiteral <|> parseVar)
  <*  (token (char '<'))
  <*> parseExpression
                                                                                                                                         
parseArith :: Parser Expr
parseArith = do
  x <- parseLiteral <|> parseVar
  rest x
  where
    rest x = 
      (do _ <- token $ char '+'
          y <- parseLiteral <|> parseVar
          rest (x :+: y)) <|>
      (do _ <- token $ char '-'
          y <- parseLiteral <|> parseVar
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
  expr <- readFile "Program.txt"
  --print $ expr
  --print $ parse parseStatements expr
  case parse parseStatements expr of 
    Just (stmts, _) -> do
      execute stmts []
      _ <- getLine
      pure ()
    _ -> do
      putStrLn "Invalid input"
      _ <- getLine
      pure ()

