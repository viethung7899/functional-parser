module JsonParser where

import Control.Applicative (Alternative (..), optional)
import Control.Monad (guard, replicateM)
import Data.Char (chr, isHexDigit)
import Data.Foldable (asum)
import Numeric (readHex)
import Parser

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonString String
  | JsonNumber Double
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving (Show, Eq)

-- Parse JSON value
jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonBool <|> jsonString <|> jsonNumber <|> jsonArray <|> jsonObject

-- Null value
jsonNull :: Parser JsonValue
jsonNull = JsonNull <$ string "null"

-- Boolean values
jsonBool :: Parser JsonValue
jsonBool = jsonTrue <|> jsonFalse
  where
    jsonTrue = JsonBool True <$ string "true"
    jsonFalse = JsonBool False <$ string "false"

-- JSON string
jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLiteral

stringLiteral :: Parser String
stringLiteral = between (char '"') (char '"') (many extChar)

extChar :: Parser Char
extChar = parseIf anyChar "normal character" ((&&) <$> (/= '"') <*> (/= '\\')) <|> escape

escape :: Parser Char
escape = char '\\' *> (unicode <|> escapeChar)

unicode :: Parser Char
unicode = string "u" *> (chr . fst . head . readHex <$> replicateM 4 (parseIf anyChar "hex character" isHexDigit))

escapeChar :: Parser Char
escapeChar =
  asum $
    readEscapeChar
      <$> zip
        ['"', '\\', '/', '\b', '\f', '\n', '\r', '\t']
        ['"', '\\', '/', 'b', 'f', 'n', 'r', 't']
  where
    readEscapeChar (c, s) = c <$ char s

-- JSON number
jsonNumber :: Parser JsonValue
jsonNumber = JsonNumber <$> number

number :: Parser Double
number = do
  s <- minus <|> pure 1
  i <- integer
  f <- fraction <|> pure 0
  e <- expo <|> pure 0
  pure (fromIntegral s * (fromIntegral i + f) * (10 ^^ e))

integer :: Parser Integer
integer = read <$> (noLeadingZero <|> (: []) <$> digit)

fraction :: Parser Double
fraction = read . ('0' :) <$> ((:) <$> char '.' <*> some digit)

expo :: Parser Integer
expo = do
  _ <- char 'e' <|> char 'E'
  sign <- plus <|> minus <|> pure 1
  val <- read <$> some digit
  pure (sign * val)

noLeadingZero :: Parser [Char]
noLeadingZero = do
  first <- digit
  guard (first /= '0')
  rest <- some digit
  pure (first : rest)

plus :: Parser Integer
plus = 1 <$ char '+'

minus :: Parser Integer
minus = (-1) <$ char '-'

-- Parse JSON array
jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> array

array :: Parser [JsonValue]
array = between (char '[') (char ']') content

content :: Parser [JsonValue]
content = (jsonElement `separateBy` char ',') <|> [] <$ spaces

jsonElement :: Parser JsonValue
jsonElement = between spaces spaces jsonValue

-- Parse JSON object
jsonObject :: Parser JsonValue
jsonObject = JsonObject <$> object

object :: Parser [(String, JsonValue)]
object = between (char '{') (char '}') ([] <$ spaces <|> members)

members :: Parser [(String, JsonValue)]
members = member `separateBy` char ','

member :: Parser (String, JsonValue)
member = do
  k <- key
  _ <- char ':'
  v <- jsonElement
  pure (k, v)

key :: Parser String
key = between spaces spaces stringLiteral

-- Parsing between enclosure "", (), []
between :: Parser a -> Parser b -> Parser c -> Parser c
between left right middle = left *> middle <* right

-- Parsing a, a, a, a, a, ...
separateBy :: Parser a -> Parser b -> Parser [a]
separateBy element separator = (:) <$> element <*> many (separator *> element) <|> pure []

-- Parse JSON from a file
parseJsonFile :: FilePath -> IO (Either ParserError JsonValue)
parseJsonFile = parseFile jsonValue