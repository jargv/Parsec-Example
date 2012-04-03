#! /usr/bin/env runhaskell

import Text.Parsec
import Text.Parsec.String
import Control.Applicative((<$>), (<*>))

main = print . parse config "Config" =<< readFile "example.txt"

{- Declare our data structures -}
type Name = String
type Field = (String, String) 
data Config = Config [Section] deriving Show
data Section = Section Name [Field] deriving Show

{- Define the top-level parser and then the sub-parsers -}
config :: Parser Config
config = Config <$> many section

section = Section <$> heading <*> many keyValuePair

heading = between (char '[') (char ']') (many $ noneOf "]")

keyValuePair = do
    spaces 
    key <- noneOf "[" `manyTill` equals
    val <- anyChar `manyTill` char '\n'
    return (key, val)
  where
    equals = spaces >> char '=' >> spaces
