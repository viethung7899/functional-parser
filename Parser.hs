module Parser where

import Control.Applicative (Alternative (..), empty, (<|>))
import Control.Monad (guard)
import Data.Char (isDigit)
import Data.Foldable (asum)

-- DEFINITION of Parser
newtype Parser a = Parser
  { parse :: String -> Maybe (a, String)
  }

-- ESTABLISH property
-- A parser need to be a functor to transform map data
-- (a -> b) -> Parser a -> Parser b
instance Functor Parser where
  fmap f (Parser pa) = Parser $ \input -> do
    (a, rest) <- pa input
    Just (f a, rest)

-- A parser need to be applicatiive to transform chaining
-- Parser (a -> b) -> Parser a -> Parser b
instance Applicative Parser where
  pure x = Parser $ \input -> Just (x, input)
  pf <*> pa = Parser $ \input -> do
    (f, next) <- parse pf input
    (a, rest) <- parse pa next
    Just (f a, rest)

-- A parser need to be a monad to chain the operation
instance Monad Parser where
  (Parser pa) >>= f = Parser $ \input -> do
    (a, next) <- pa input
    parse (f a) next

instance MonadFail Parser where
  fail _ = Parser $ const Nothing

-- Implement Alternative to perform switch control
instance Alternative Parser where
  empty = fail ""
  (Parser p1) <|> (Parser p2) = Parser (\input -> p1 input <|> p2 input)

-- PARSING TEXT
-- Parse any character
anyChar :: Parser Char
anyChar = Parser pChar
  where
    pChar [] = Nothing
    pChar (x : xs) = Just (x, xs)

-- Parse a character
char :: Char -> Parser Char
char c = anyChar `satisfy` (c ==)

-- Parse a string
-- A bit magical in this part
--- mapM :: (Traversible t, Monad m) => (a -> m b) -> t a -> m (t b)
--- this functiion perform monadic operation from left to right, and collect the result
--- If t is the list container (or []), m is the Parser, then mapM becomes, a and b are both char
----- mapM signaturer becomes (Char -> Parser Char) -> [Char] -> Parser [Char]
string :: String -> Parser String
string = mapM char

-- Parse a whitespaces
-- space = asum [char ' ', char '\n', char '\t', char '\r']
space :: Parser Char
space = asum $ map char [' ', '\n', '\t', '\r']

spaces :: Parser String
spaces = many space

-- Parse digit
digit :: Parser Char
digit = anyChar `satisfy` isDigit

-- Stop the parser when condition is met
satisfy :: Parser a -> (a -> Bool) -> Parser a
satisfy pa predicate = do
  a <- pa
  guard $ predicate a
  pure a

-- Parse a file
parseFile :: Parser a -> FilePath -> IO (Maybe a)
parseFile parser filename = do
  input <- readFile filename
  return $ fst <$> parse parser input