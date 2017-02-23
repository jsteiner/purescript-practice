module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION, throw)

import Text.Parsing.StringParser (Parser, runParser)
import Text.Parsing.StringParser.Combinators (many)
import Text.Parsing.StringParser.String (anyDigit, skipSpaces, oneOf)
import Data.Int (fromString)
import Data.Maybe (fromMaybe)
import Data.List (List, toUnfoldable, length, filter)
import Data.Either (Either(..))
import Data.String (fromCharArray)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)

main :: forall e. Eff (console :: CONSOLE, err :: EXCEPTION, fs :: FS | e) Unit
main =
  log <<< show =<< countTriangles =<< readTextFile UTF8 "input"

countTriangles :: forall e. String -> Eff (err :: EXCEPTION | e) Int
countTriangles ls =
  case runParser parsePotentialTriangles ls of
    Left _ -> throw "Failed to parse input"
    Right ts -> pure $ sumTrue ts

sumTrue :: List Boolean -> Int
sumTrue = length <<< filter id

parsePotentialTriangles :: Parser (List Boolean)
parsePotentialTriangles = many parsePotentialTriangle

parsePotentialTriangle :: Parser Boolean
parsePotentialTriangle = do
  skipSpaces
  first <- parseLength
  skipSpaces
  second <- parseLength
  skipSpaces
  third <- parseLength
  oneOf ['\n']

  pure $ isTriangle first second third

parseLength :: Parser Int
parseLength =
  fromMaybe 0
  <<< fromString
  <<< fromCharArray
  <<< toUnfoldable
  <$> many anyDigit

isTriangle :: Int -> Int -> Int -> Boolean
isTriangle a b c = a + b > c && a + c > b && b + c > a
