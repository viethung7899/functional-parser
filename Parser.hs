{-# LANGUAGE FlexibleInstances #-}

module Parser where

import Control.Applicative (Alternative (..), empty, (<|>))
import Control.Monad (guard)
import Data.Char (isDigit)
import Data.Foldable (asum)

type Location = (Int, Int)

data Input = Input
  { inputLocation :: Location,
    inputString :: String
  }
  deriving (Show, Eq)

getCharFromInput :: Input -> Maybe (Char, Input)
getCharFromInput (Input _ []) = Nothing
getCharFromInput (Input (l, p) (x : xs)) =
  if x == '\n'
    then Just (x, Input (l + 1, 1) xs)
    else Just (x, Input (l, p + 1) xs)

data ParserError = ParserError Location String deriving (Show)

makeInput :: String -> Input
makeInput = Input (1, 1)

-- DEFINITION of Parser
newtype Parser a = Parser
  { parse :: Input -> Either ParserError (a, Input)
  }

-- ESTABLISH property
-- A parser need to be a functor to transform map data
-- (a -> b) -> Parser a -> Parser b
instance Functor Parser where
  fmap f (Parser pa) = Parser $ \input -> do
    (a, input') <- pa input
    return (f a, input')

-- A parser need to be applicatiive to transform chaining
-- Parser (a -> b) -> Parser a -> Parser b
instance Applicative Parser where
  pure x = Parser $ \input -> Right (x, input)
  (Parser pf) <*> (Parser pa) = Parser $ \input -> do
    (f, next) <- pf input
    (a, rest) <- pa next
    return (f a, rest)

-- A parser need to be a monad to chain the operation
instance Monad Parser where
  (Parser pa) >>= f = Parser $ \input -> do
    (a, next) <- pa input
    parse (f a) next

-- Implement Alternative to perform switch control
instance Alternative (Either ParserError) where
  empty = Left $ ParserError (1, 1) "empty"
  Left _ <|> e = e
  e <|> _ = e

instance Alternative Parser where
  empty = Parser $ const empty
  (Parser p1) <|> (Parser p2) = Parser (\input -> p1 input <|> p2 input)

-- PARSING TEXT
-- Parse any character
anyChar :: Parser Char
anyChar = Parser $ \input ->
  case getCharFromInput input of
    Just x -> Right x
    Nothing -> Left $ ParserError (inputLocation input) "end of string" -- No more character to parse

-- Parse a character
char :: Char -> Parser Char
char c = parseIf anyChar ("character '" ++ [c] ++ "'") (c ==)

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
digit = parseIf anyChar "digit" isDigit

-- Stop the parser when condition is met
parseIf :: (Show a) => Parser a -> String -> (a -> Bool) -> Parser a
parseIf pa desc predicate = Parser $ \input ->
  case parse pa input of
    Right (x, input')
      | predicate x -> Right (x, input')
      | otherwise ->
        Left $
          ParserError
            (inputLocation input)
            ("Expected: " ++ desc ++ ", but found " ++ show x)
    _ -> Left $ ParserError (inputLocation input) ("Expected: " ++ desc ++ ", but reached end of string")

-- Parse a file
parseFile :: Parser b -> FilePath -> IO (Either ParserError b)
parseFile parser filename = do
  input <- readFile filename
  return $ fst <$> parse parser (makeInput input)