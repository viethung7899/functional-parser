module TestUtil where

import Data.Either (rights)
import Parser
  ( Input (inputString),
    Parser (parse),
    ParserError,
    makeInput,
  )
import Test.QuickCheck (Arbitrary (arbitrary), Gen, suchThat)

--Extraction function
checkResult :: (Eq a) => Either ParserError (a, Input) -> a -> String -> Bool
checkResult output result remain = checkParseResult output result && checkRemainInput output remain

checkParseResult :: Eq a => Either ParserError (a, b) -> a -> Bool
checkParseResult output result = (fst . head $ rights [output]) == result

checkRemainInput :: Either ParserError (a, Input) -> String -> Bool
checkRemainInput output remainText = (inputString . snd . head $ rights [output]) == remainText

parseInput :: Parser a -> String -> Either ParserError (a, Input)
parseInput p s = parse p (makeInput s)

nonEmptyString :: Gen String
nonEmptyString = arbitrary `suchThat` (not . null)