module Util.Parser (Parser, parse) where

import Text.Parsec (Parsec)
import qualified Text.Parsec as P

type Parser = Parsec String ()

parse :: Parser a -> String -> a
parse p = either (error . show) id . P.parse p ""
