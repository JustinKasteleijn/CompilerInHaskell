module AST where

type Env = [(String, Val)]

data Statement 
  = If Expr [Statement] [Statement]
  | Assignment String Expr 
  | Print Expr 
  | While Expr [Statement]
 deriving (Show)

data Expr 
  = Literal Val
  | Var String 
  | Expr :+:  Expr
  | Expr :-:  Expr
  | Expr :*:  Expr
  | Expr :/:  Expr
  | Expr :==: Expr 
  | Expr :!=: Expr
  | Expr :<:  Expr
  | Expr :>:  Expr 
 deriving (Show)
  
data Val 
  = ValInt Int 
  | ValBool Bool
  | ValDouble Double
  | ValList [Val]
 deriving (Show, Eq, Ord)

eval :: Expr -> Env -> Maybe Val
eval (Literal x) env       = Just $ x
eval (Var s) env           = lookup s env
eval (expr :+: expr') env  = do 
  x <- eval expr env
  y <- eval expr' env
  case (x, y) of
    (ValInt x', ValInt y')       -> return $ ValInt (x' + y')
    (ValDouble x', ValDouble y') -> return $ ValDouble (x' + y')
    (ValInt x', ValDouble y')    -> return $ ValDouble (fromIntegral x' + y')
    (ValDouble x', ValInt y')    -> return $ ValDouble (x' + fromIntegral y')
    _ -> Nothing 
eval (expr :-: expr') env  = do 
  x <- eval expr env
  y <- eval expr' env
  case (x, y) of
    (ValInt x', ValInt y')       -> return $ ValInt (x' - y')
    (ValDouble x', ValDouble y') -> return $ ValDouble (x' - y')
    (ValInt x', ValDouble y')    -> return $ ValDouble (fromIntegral x' - y')
    (ValDouble x', ValInt y')    -> return $ ValDouble (x' - fromIntegral y')
    _ -> Nothing 
eval (expr :*: expr') env  = do 
  x <- eval expr env
  y <- eval expr' env
  case (x, y) of
    (ValInt x', ValInt y')       -> return $ ValInt (x' * y')
    (ValDouble x', ValDouble y') -> return $ ValDouble (x' * y')
    (ValInt x', ValDouble y')    -> return $ ValDouble (fromIntegral x' * y')
    (ValDouble x', ValInt y')    -> return $ ValDouble (x' * fromIntegral y')
    _ -> Nothing 
eval (expr :/: expr') env  = do 
  x <- eval expr env
  y <- eval expr' env
  case (x, y) of
    (ValInt x', ValInt y')       -> return $ ValInt (x' `div` y')
    (ValDouble x', ValDouble y') -> return $ ValDouble (x' / y')
    (ValInt x', ValDouble y')    -> return $ ValDouble (fromIntegral x' / y')
    (ValDouble x', ValInt y')    -> return $ ValDouble (x' / fromIntegral y')
    _ -> Nothing 
eval (expr :==: expr') env = do 
  x <- eval expr env
  y <- eval expr' env
  return $ ValBool $ x == y  
eval (expr :!=: expr') env = do 
  x <- eval expr env
  y <- eval expr' env 
  return $ ValBool $ x /= y
eval (expr :<: expr') env  = do 
  x <- eval expr env
  y <- eval expr' env 
  case (x, y) of
    (ValInt x', ValInt y')       -> return $ ValBool (x' < y')
    (ValDouble x', ValDouble y') -> return $ ValBool (x' < y')
    (ValInt x', ValDouble y')    -> return $ ValBool (fromIntegral x' < y')
    (ValDouble x', ValInt y')    -> return $ ValBool (x' < fromIntegral y')
    _ -> return $ ValBool $ x < y 
eval (expr :>: expr') env  = do 
  x <- eval expr env
  y <- eval expr' env 
  return $ ValBool $ x > y

execute :: [Statement] -> Env -> IO Env
execute [] env = pure env  
execute (stmt:stmts) env = do 
  newEnv <- case stmt of 
    (Assignment var expr) -> 
      case eval expr env of  
        Just val -> pure ((var, val) : env)  
        Nothing  -> do
          print $ "ERROR: Invalid assignment to " ++ var
          pure env  
    (If cond ifbody elsebody) -> do 
      case eval cond env of 
        Just (ValBool True)  -> execute ifbody env  
        Just (ValBool False) -> execute elsebody env 
        _                    -> do
          print $ "ERROR: Condition must be of type Boolean"
          pure env
    (Print expr) -> 
      case eval expr env of
          Just val -> printVal val *> pure env 
          Nothing  -> pure env  
    (While cond body) -> do 
      case eval cond env of 
        Just (ValBool True) -> do 
          updatedEnv <- execute body env
          execute [(While cond body)] updatedEnv  
        Just (ValBool False) -> pure env  
        _                    -> do
          print $ "ERROR: Condition must be of type Boolean"
          pure env
  execute stmts newEnv


printVal :: Val -> IO ()
printVal (ValInt x)    = print x 
printVal (ValBool x)   = print x 
printVal (ValDouble x) = print x
printVal (ValList xs)  = mapM print xs *> pure ()

--main :: IO ()
--main = do
--  let program = [
--          Assignment "x" (Literal (ValInt 5)),   
--          If (Literal (ValBool False)) 
--             ((Var "x") :+: (Literal (ValInt 10))) 
--              (Var "x"),
--          If (Literal (ValBool True)) 
--              ((Literal (ValInt 2)) :+: (Literal (ValInt 3))) 
--              (Literal (ValInt 7))
--        ]
--  execute program []
