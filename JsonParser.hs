module JsonParser (parse, jsonValue, parseJsonFile) where

import Control.Applicative (Alternative (..), optional)
import Control.Monad
import Data.Foldable (asum)
import Data.Char
import Numeric
import Parser (Parser (..), anyChar, char, digit, integer, parse, satisfy, spaces, string, parseFile, unsignedInt, zeroLeadingInt)
import System.IO (NewlineMode (inputNL))
import System.Posix.DynamicLinker.ByteString (DL (Next))
import GHC.Float (divideDouble)

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonString String
  | JsonNumber Rational
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving (Show, Eq)

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
escapeChar :: Parser Char
escapeChar = asum $ readEscapeChar <$>
  zip ['"', '\\', '/', '\b', '\f', '\n', '\r', '\t']
      ['"', '\\', '/', 'b', 'f', 'n', 'r', 't']
  where readEscapeChar (c, s) = c <$ char s

unicode :: Parser Char
unicode = string "u" *> (chr . fst . head . readHex <$> replicateM 4 (anyChar `satisfy` isHexDigit))

escape :: Parser Char
escape = char '\\' *> (unicode <|> escapeChar)

anyExtChar :: Parser Char
anyExtChar = (anyChar `satisfy` ((&&) <$> (/= '"') <*> (/= '\\'))) <|> escape

stringLiteral :: Parser String
stringLiteral = between (char '"') (char '"') jsonCharacters
  where
    jsonCharacters = many (anyExtChar `satisfy` ('"' /=))

jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLiteral

-- JSON number
fraction :: Parser Rational
fraction = char '.' *> (foldr (\n d -> (n + d) / 10) 0 <$> some (toRational <$> digit))
  <|> pure 0

expo :: Parser Integer
expo = do
  _ <- char 'e' <|> char 'E'
  sign <- 1 <$ char '+' <|> (-1) <$ char '-'
  val <- zeroLeadingInt
  pure (sign * val)
  <|> pure 0

number :: Parser Rational
number =
  formFraction <$> integer <*> fraction <*> expo
  where formFraction i f e = (fromIntegral i + f) * (10 ^^ e)

jsonNumber :: Parser JsonValue
jsonNumber = JsonNumber <$> undefined

-- Parse JSON value
jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonBool <|> jsonString <|> jsonNumber <|> jsonArray <|> jsonObject

-- Parse JSON array
jsonElement :: Parser JsonValue
jsonElement = between spaces spaces jsonValue

array :: Parser [JsonValue]
array = between (char '[') (char ']') (jsonElements <|> [] <$ spaces)
  where
    jsonElements = jsonElement `separateBy` char ','

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> array

-- Parse JSON object
key :: Parser String
key = between spaces spaces stringLiteral

member :: Parser (String, JsonValue)
member = do
  k <- key
  _ <- char ':'
  v <- jsonElement
  pure (k, v)

members :: Parser [(String, JsonValue)]
members = member `separateBy` char ','

object :: Parser [(String, JsonValue)]
object = between (char '{') (char '}') (members <|> [] <$ spaces)

jsonObject :: Parser JsonValue
jsonObject = JsonObject <$> object

-- Parsing between enclosure "", (), []
between :: Parser a -> Parser b -> Parser c -> Parser c
between left right middle = left *> middle <* right

-- Parsing a, a, a, a, a, ...
separateBy :: Parser a -> Parser b -> Parser [a]
separateBy pa pb = do
  ma <- optional pa
  mb <- optional pb
  case (ma, mb) of
    (Just a, Just d) -> do
      as <- pa `separateBy` pb
      pure (a : as)
    (Just a, _) -> pure [a]
    _ -> empty

parseJsonFile :: FilePath -> IO (Maybe JsonValue)
parseJsonFile = parseFile jsonValue