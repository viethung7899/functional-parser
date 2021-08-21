{-# LANGUAGE FlexibleContexts #-}

module ParserTest where

import Data.Char (isDigit, isSpace)
import Data.Either (isLeft, isRight)
import Parser
  ( Parser (parse),
    anyChar,
    char,
    digit,
    makeInput,
    space,
    spaces,
    string,
  )
import Test.Hspec (describe, hspec, it, shouldSatisfy)
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    Gen,
    Property,
    Testable (property),
    choose,
    elements,
    forAll,
    listOf,
    suchThat,
    (==>),
  )
import Test.QuickCheck.Arbitrary (Arbitrary)
import TestUtil
  ( checkParseResult,
    checkRemainInput,
    checkResult,
    parseInput,
    nonEmptyString,
    parseEmptyString
  )

main :: IO ()
main = hspec $ do
  describe "parse" $ do
    describe "any character" $ do
      it "parse on non-empty string" $ do
        property $ forAll nonEmptyString (isRight . parse anyChar . makeInput)
      it "parse on empty string" $ do
        parseEmptyString anyChar
    describe "character" $ do
      it "parse matching characters" $ do
        property testMatchingCharacter
      it "parse mismatch characters" $ do
        property testMismatchCharacter
      it "parse characters on empty string" $ do
        property testCharacterOnEmptyString
    describe "string" $ do
      it "parse on empty string" $ do
        property $ forAll nonEmptyString (parseEmptyString . string)
      it "parse matching string" $ do
        property testMatchingString
      it "parse non-matching string" $ do
        property testMismatchingString
    describe "digit" $ do
      it "parse on empty string" $ do
        parseEmptyString digit
      it "parse on non-empty string starts with digit character" $ do
        property testOnStringWithDigit
      it "parse on non-empty string starts with non-digit character" $ do
        property testOnStringWithoutDigit
    describe "space" $ do
      it "parse space on empty string" $ do
        parseEmptyString space
      it "parse one space on the string without spaces" $ do
        property testSpaceOnStringWithoutSpaces
      it "parse multiple spaces on the string without spaces" $ do
        property testSpacesOnStringWithoutSpaces
      it "parse multiple spaces on space-beginning string" $ do
        property testSpacesOnStringWithSpaces

-- | | Testing function definitions


-- Parse characters
testMatchingCharacter :: Char -> String -> Bool
testMatchingCharacter c text = checkParseResult output c && checkRemainInput output text
  where
    output = parse (char c) (makeInput (c : text))

testMismatchCharacter :: Property
testMismatchCharacter = forAll mismatchCharAndString $ \(c, s) -> isLeft $ parse (char c) (makeInput s)

mismatchCharAndString :: Gen (Char, String)
mismatchCharAndString = ((,) <$> arbitrary <*> nonEmptyString) `suchThat` (\(c, s) -> head s /= c)

testCharacterOnEmptyString :: Char -> Bool
testCharacterOnEmptyString c = isLeft $ parse (char c) (makeInput "")

-- Parse string
testMatchingString :: String -> String -> Bool
testMatchingString s text = checkResult output s text
  where
    output = parse (string s) (makeInput (s ++ text))

testMismatchingString :: String -> String -> String -> Property
testMismatchingString s ns text = (s /= ns && not (null s)) ==> isLeft output
  where
    output = parse (string s) (makeInput (ns ++ text))

-- Parse digit
testOnStringWithDigit :: Property
testOnStringWithDigit = forAll digitAndString $ \(d, s) -> checkResult (parseInput digit (d : s)) d s

digitAndString :: Gen (Char, String)
digitAndString = (,) <$> choose ('0', '9') <*> arbitrary

testOnStringWithoutDigit :: Property
testOnStringWithoutDigit = forAll stringNoDigits $ \s -> isLeft $ parseInput digit s

stringNoDigits :: Gen String
stringNoDigits = dropWhile isDigit <$> arbitrary

-- Space
testSpaceOnStringWithoutSpaces :: String -> Property
testSpaceOnStringWithoutSpaces s = forAll stringWithoutSpaces $ \s -> isLeft $ parseInput space s

testSpacesOnStringWithoutSpaces :: Property
testSpacesOnStringWithoutSpaces = forAll stringWithoutSpaces $ \s -> checkResult (parseInput spaces s) "" s

testSpacesOnStringWithSpaces :: Property
testSpacesOnStringWithSpaces = forAll stringWithSpaces $ \(sp, s) -> checkResult (parseInput spaces (sp ++ s)) sp s

stringWithSpaces :: Gen (String, String)
stringWithSpaces = (,) <$> (listOf . elements) [' ', '\n', '\t', '\r'] <*> stringWithoutSpaces

stringWithoutSpaces :: Gen String
stringWithoutSpaces = dropWhile isSpace <$> arbitrary