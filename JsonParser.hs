module JsonParser (parse, jsonValue, parseJsonFile) where

import Control.Applicative (Alternative (..), optional)
import Data.Foldable (asum)
import Parser (Parser (..), anyChar, char, integer, parse, satisfy, spaces, string, parseFile)
import System.IO (NewlineMode (inputNL))
import System.Posix.DynamicLinker.ByteString (DL (Next))

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonString String
  | JsonNumber Integer
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving (Show, Eq)

-- Parse null value
jsonNull :: Parser JsonValue
jsonNull = JsonNull <$ string "null"

-- Parse boolean values
jsonBool :: Parser JsonValue
jsonBool = jsonTrue <|> jsonFalse
  where
    jsonTrue = JsonBool True <$ string "true"
    jsonFalse = JsonBool False <$ string "false"

-- Parse JSON string
stringInQuote :: Parser String
stringInQuote = between (char '"') (char '"') jsonCharacters
  where
    jsonCharacters = many (anyChar `satisfy` ('"' /=))

jsonString :: Parser JsonValue
jsonString = JsonString <$> stringInQuote

-- Parse JSON number
jsonNumber :: Parser JsonValue
jsonNumber = JsonNumber <$> integer

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
key = between spaces spaces stringInQuote

member :: Parser (String, JsonValue)
member = do
  k <- key
  _ <- char ':'
  v <- jsonElement
  pure (k, v)

members :: Parser [(String, JsonValue)]
members = member `separateBy` char ','

object :: Parser [(String, JsonValue)]
object = between (char '{') (char '}') (members <|> ([] <$ spaces))

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