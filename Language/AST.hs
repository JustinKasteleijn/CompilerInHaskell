
data Expr 
  = Literal Val
  | Expr :+: Expr
  | Expr :-: Expr
  | Expr :==: Expr 
  | If Expr Expr Expr
  
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
  y <- eval expr; 
  return $ ValBool $ x == y  
eval (If cond expr expr') = 
  case eval cond of
      Just (ValBool True)  -> eval expr 
      Just (ValBool False) -> eval expr'
      _                    -> error "If statement is not a bool"

main :: IO ()
main = do 
  print $ eval (If (Literal (ValBool False)) ((Literal (ValInt 5)) :+: (Literal (ValInt 10))) (Literal (ValInt 5))) 
