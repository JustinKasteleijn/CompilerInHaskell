module Parser where

import Control.Applicative (Alternative(..))
import Data.Char (isSpace, isDigit, isAlphaNum, isAlpha)

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

-- Instances
instance Functor Parser where 
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f px = 
    Parser $ \input -> do
      (x, rest) <- parse px input 
      Just (f x, rest)

instance Applicative Parser where 
  pure :: a -> Parser a
  pure x = 
    Parser $ \input -> 
      Just (x, input)
      
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> px = 
    Parser $ \input -> do
      (f, rest)  <- parse pf input
      (x, rest') <- parse px rest 
      Just (f x, rest')

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  px >>= f = 
    Parser $ \input -> do 
      (x, rest) <- parse px input 
      parse (f x) rest
      
instance MonadFail Parser where 
  fail :: String -> Parser a
  fail _ = empty
      
instance Alternative Parser where 
  empty :: Parser a 
  empty = 
    Parser $ \_ -> Nothing 
    
  (<|>) :: Parser a -> Parser a -> Parser a 
  px <|> py = 
    Parser $ \input -> 
      parse px input <|> parse py input 

  some :: Parser a -> Parser [a]
  some p = some'
    where 
      many' = some' <|> pure []
      some' = (:) <$> p <*> many'
  
  many :: Parser a -> Parser [a]
  many p = many'
    where 
      many' = some' <|> pure []
      some' = (:) <$> p <*> many'

-- Parser Functions 
item :: Parser Char 
item = 
  Parser $ \input -> 
    case input of 
        []     -> Nothing
        (x:xs) -> Just (x, xs)

char :: Char -> Parser Char 
char c = Parser charP
  where 
    charP :: String -> Maybe (Char, String)
    charP []      = Nothing
    charP (x:xs) 
      | x == c    = Just (c, xs)
      | otherwise = Nothing

space :: Parser Char
space = sat (\c -> c == ' ' || c == '\t')      

digit :: Parser Char 
digit = sat isDigit

alphaNum :: Parser Char 
alphaNum = sat isAlphaNum

alpha :: Parser Char
alpha = sat isAlpha

newline :: Parser ()
newline = (char '\n'
  <|> char '\r')
  *> pure ()

token :: Parser a -> Parser a
token p = whitespace *> p <* whitespace

nat :: Parser Int 
nat = read <$> some digit

int :: Parser Int 
int = nat <|> char '-' *> (negate <$> nat)
    
string :: String -> Parser String 
string = mapM char

indented :: Parser ()
indented = string "  " *> pure ()

sat :: (Char -> Bool) -> Parser Char
sat pred = 
  Parser $ \input -> do
    (x, rest) <- parse item input
    if pred x
      then Just (x, rest)
      else Nothing

whitespace :: Parser () 
whitespace = 
  many space *> pure ()
  
lineBreak :: Parser ()
lineBreak = many space *> newline *> pure ()
  
sepBy1 :: Parser a -> Parser b -> Parser [a] 
sepBy1 px psep = (:) 
  <$> px 
  <*> many (psep *> px) 

sepBy :: Parser a -> Parser b -> Parser [a] 
sepBy px psep = sepBy1 px psep <|> pure []

try :: Parser a -> Parser a 
try p = 
  Parser $ \input -> 
    case parse p input of
      Just (x, rest) -> Just (x, rest)
      Nothing        -> Nothing 

--main :: IO ()
--main = do
--  print $ parse (sepBy nat (char ',')) ""
