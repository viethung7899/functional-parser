module JsonParser (parse, json) where

import Control.Applicative ( Alternative(..), optional )
import Parser ( Parser(..), parse, char, anyChar, string, integer, satisfy, spaces )
import Data.Foldable (asum)
import System.IO (NewlineMode(inputNL))
import System.Posix.DynamicLinker.ByteString (DL(Next))

data JsonValue =
  JsonNull
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
jsonBool = jsonTrue <|> jsonFalse where
  jsonTrue = JsonBool True <$ string "true"
  jsonFalse = JsonBool False <$ string "false"

-- Parse JSON string
token :: Parser String
token = between (many jsonCharacter) (char '"') (char '"')
  where jsonCharacter = anyChar `satisfy` ('"' /=)

jsonString :: Parser JsonValue
jsonString = JsonString <$> token

-- Parse JSON number
jsonNumber :: Parser JsonValue
jsonNumber = JsonNumber <$> integer


-- Parse JSON value
jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonBool <|> jsonString <|> jsonNumber <|> jsonArray <|> jsonObject

-- Parse JSON array
jsonElement :: Parser JsonValue
jsonElement = between jsonValue spaces spaces

array :: Parser [JsonValue]
array = between jsonElements (char '[') (char ']') where
  jsonElements = jsonElement `separateBy` char ','

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> (array <|> emptyArray)

emptyArray :: Parser [JsonValue]
emptyArray =  between ([] <$ spaces) (char '[') (char ']')

-- Parse JSON object
key :: Parser String
key = between token spaces spaces

member :: Parser (String, JsonValue)
member = do
  k <- key
  _ <- char ':'
  v <- jsonElement
  pure (k,v)

members :: Parser [(String, JsonValue)]
members = member `separateBy` char ','

emptyObject :: Parser [(String, JsonValue)]
emptyObject = between ([] <$ spaces) (char '{') (char '}')

object :: Parser [(String, JsonValue)]
object = between members (char '{') (char '}')

jsonObject :: Parser JsonValue
jsonObject = JsonObject <$> (emptyObject <|> object)


-- Parsing between enclosure "", (), []
between :: Parser a -> Parser b -> Parser c -> Parser a
between mid left right = left *> mid <* right

-- Parsing a, a, a, a, a, ...
separateBy :: Parser a -> Parser b -> Parser [a]
separateBy pa delimeter = do
  ma <- optional pa
  md <- optional delimeter
  case (ma, md) of
    (Just a, Just d) -> do
      as <- pa `separateBy` delimeter
      pure (a:as)
    (Just a, _) -> pure [a]
    _ -> empty


json :: Parser JsonValue
json = between jsonValue spaces spaces