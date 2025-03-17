module AST where

data Statement 
  = If Expr Expr Expr
 deriving (Show)

data Expr 
  = Literal Val
  | Expr :+: Expr
  | Expr :-: Expr
  | Expr :==: Expr 
 deriving (Show)
  
data Val 
  = ValInt Int 
  | ValBool Bool
 deriving (Show, Eq)

eval :: Expr -> Maybe Val
eval (Literal x)          = Just $ x
eval (expr :+: expr')     = do 
  (ValInt x) <- eval expr 
  (ValInt y) <- eval expr'
  return $ ValInt $ x + y
eval (expr :-: expr')     = do 
  (ValInt x) <- eval expr 
  (ValInt y) <- eval expr'
  return $ ValInt $ x - y
eval (expr :==: expr')    = do 
  x <- eval expr 
  y <- eval expr'
  return $ ValBool $ x == y  

execute :: [Statement] -> IO ()
execute []           = pure ()
execute (stmt:stmts) = do 
  case stmt of 
    (If cond expr expr') -> do 
      case eval cond of 
        Just (ValBool True)  -> print $ eval expr
        Just (ValBool False) -> print $ eval expr'
        _                    -> error "Condition must be of type Boolean"
  execute stmts

--main :: IO ()
--main = do
--  let program = [
--          If (Literal (ValBool False)) 
--              ((Literal (ValInt 5)) :+: (Literal (ValInt 10))) 
--              (Literal (ValInt 5)),
--          If (Literal (ValBool True)) 
--              ((Literal (ValInt 2)) :+: (Literal (ValInt 3))) 
--              (Literal (ValInt 7))
--        ]
--  execute program
