import Parser 
import AST
import Control.Applicative (Alternative(..))

parseStatements :: Parser [Statement]
parseStatements = some (parseStatement <* optional lineBreak)

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
parseWhile = do
  _     <- string "while"
  cond  <- between (whitespace *> char '(') (char ')' <* newline) parseExpression
  stmts <- indentedStatements 
  return $ While cond stmts
  
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
  <|> parseExpr

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
                                                                                                                                         
parseAdd :: Parser (Expr -> Expr -> Expr) 
parseAdd = (:+:) <$ (whitespace *> char '+' *> whitespace)
   
parseSub :: Parser (Expr -> Expr -> Expr)  
parseSub = (:-:) <$ (whitespace *> char '-' *> whitespace)
 
parseDiv :: Parser (Expr -> Expr -> Expr) 
parseDiv = (:/:) <$ (whitespace *> char '/' *> whitespace)
   
parseMul :: Parser (Expr -> Expr -> Expr) 
parseMul = (:*:) <$ (whitespace *> char '*' *> whitespace)
   
parseParens :: Parser Expr
parseParens = between (whitespace *> char '(' *> whitespace) 
                      (whitespace *> char ')' *> whitespace) 
                      parseExpr
   
parseFactor :: Parser Expr
parseFactor = parseLiteral 
  <|> parseParens 
  <|> parseVar

parseTerm :: Parser Expr
parseTerm = do
    left <- parseFactor
    rest left
  where
    rest left = (do
                    op <- parseMul <|> parseDiv
                    right <- parseFactor
                    rest (op left right))
                <|> return left

parseExpr :: Parser Expr
parseExpr = do
    left <- parseTerm
    rest left
  where
    rest left = (do
                    op <- parseAdd <|> parseSub
                    right <- parseTerm
                    rest (op left right))
                <|> return left
      
parseVar :: Parser Expr 
parseVar = Var <$> (some (alphaNum))

parseLiteral :: Parser Expr 
parseLiteral = (Literal <$> parseVal)

parseList :: Parser [Val]
parseList = between (char '[') (char ']') (sepBy parseVal (char ',' <* whitespace))

parseDouble :: Parser Val 
parseDouble = do 
  num <- digits
  _ <- char '.'
  frac <- digits
  return $ ValDouble $ read (num ++ "." ++ frac) 
 where 
   digits :: Parser String 
   digits = some digit

parseVal :: Parser Val 
parseVal = (ValBool True <$ string "true") 
  <|> (ValBool False <$ string "false")
  <|> parseDouble
  <|> (ValInt <$> int)
  <|> (ValList <$> parseList)

main :: IO ()
main = do 
  expr <- readFile "Program.txt"
  print $ expr
  print $ parse parseStatements expr
  case parse parseStatements expr of 
    Just (stmts, _) -> do
      execute stmts []
      _ <- getLine
      pure ()
    _ -> do
      putStrLn "Invalid input"
      _ <- getLine
      pure ()

