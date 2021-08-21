module JsonParserTest where

import Data.Bifunctor
import Data.Char
import Data.Either
import Data.List.NonEmpty (nonEmpty)
import JsonParser
import Numeric
import Test.Hspec
import Test.QuickCheck
import TestUtil

main :: IO ()
main = hspec $ do
  describe "JS bool" $ do
    it "always parse string with `true`" $ do
      property $ testJsonBool True "true"
    it "always parse string with `false`" $ do
      property $ testJsonBool False "false"
    it "fails to parse `true` with incorrect letter case" $ do
      parseInput jsonBool "True " `shouldSatisfy` isLeft
    it "fails to parse `fasle` with incorrect letter case" $ do
      parseInput jsonBool "False " `shouldSatisfy` isLeft
  describe "JS null" $ do
    it "always parse string with `null`" $ do
      property testJsonNull
    it "fails to parse `null` with incorrect letter case" $ do
      parseInput jsonNull "Null " `shouldSatisfy` isLeft
  describe "JS string" $ do
    describe "escape sequences" $ do
      it "always parse with correct format" $ do
        property testEscape
      it "fails when missing '\\'" $ do
        property testEscapeWithoutBackslash
      it "fails when \"\\u\" does not follow the hex code" $ do
        property testNoHexCode
      it "fails when '\\' does not follow the hex code or escape code" $ do
        property testNoEscapeCode
    describe "srting" $ do
      it "fails when parse empty string" $ do
        parseEmptyString jsonString
      it "always parse character from string quotes and correct format" $ do
        property testValidStringLiteral
      it "always parse character from string without quotes" $ do
        property testStringNoQuote

------ Test functions
---- Boolean
testJsonBool :: Bool -> String -> String -> Bool
testJsonBool b s str = checkResult output (JsonBool b) str
  where
    output = parseInput jsonBool (s ++ str)

---- Null
testJsonNull :: String -> Bool
testJsonNull s = checkResult output JsonNull s
  where
    output = parseInput jsonNull ("null" ++ s)

---- String
randomUnicode :: Gen (Char, String)
randomUnicode = do
  hex <- vectorOf 4 (arbitrary `suchThat` isHexDigit)
  let c = (chr . fst . head . readHex) hex
  return (c, 'u' : hex)

-- Escape
randomSpecialEscape :: Gen (Char, String)
randomSpecialEscape =
  elements
    ( (\(c, s) -> (c, [s]))
        <$> zip
          ['"', '\\', '/', '\b', '\f', '\n', '\r', '\t']
          ['"', '\\', '/', 'b', 'f', 'n', 'r', 't']
    )

randomEscape :: Gen (Char, String)
randomEscape = oneof [randomUnicode, randomSpecialEscape]

testEscape :: Property
testEscape = forAll stringStartWithEspace $ \(c, s, str) -> checkResult (parseInput escape ('\\' : s ++ str)) c str
  where
    stringStartWithEspace = (\(c, s) str -> (c, s, str)) <$> randomEscape <*> (arbitrary :: Gen String)

testEscapeWithoutBackslash :: Property
testEscapeWithoutBackslash = forAll stringWithNoBacklash $ \s -> isLeft (parseInput escape s)
  where
    stringWithNoBacklash = dropWhile (== '\\') <$> arbitrary

testNoHexCode :: Property
testNoHexCode = forAll nonHexCode $ \s -> isLeft (parseInput escape ('\\' : 'u' : s))
  where
    nonHexCode = vectorOf 4 (arbitrary :: Gen Char) `suchThat` (or . fmap (not . isHexDigit))

testNoEscapeCode :: Property
testNoEscapeCode = forAll nonEscape $ \s -> isLeft (parseInput escape ('\\' : s))
  where
    nonEscape = nonEmptyString `suchThat` (\s -> head s `notElem` ['"', '\\', '/', 'b', 'f', 'n', 'r', 't', 'u'])

-- String Literal
testValidStringLiteral :: Property
testValidStringLiteral = forAll validString $ \(s, sl) -> checkResult (parseInput jsonString ("\"" ++ sl ++ "\"")) (JsonString s) ""

testStringNoQuote :: Property
testStringNoQuote = forAll validString $ \(_, sl) -> parseInput jsonString sl `shouldSatisfy` isLeft

validNormalChar :: Gen (Char, String)
validNormalChar = (\c -> (c, [c])) <$> arbitrary `suchThat` isValidChar

validEsapce :: Gen (Char, String)
validEsapce = second ('\\' :) <$> randomEscape

validString :: Gen (String, String)
validString = foldr (\(c, s) (str, strLit) -> (c : str, s ++ strLit)) ([], []) <$> listOf (oneof [validEsapce, validNormalChar])

-- Number
validNumber :: Gen (Double, String)
validNumber = undefined
