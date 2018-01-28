module AST where

import qualified Data.Set as S  
import Text.ParserCombinators.Parsec hiding ((<|>), many)
import Control.Applicative
import Control.Monad
import Data.Char ( isSpace )

type CharSet = S.Set Char

data RE =
    CharClass CharSet
  | Seq [RE]
  | Alt RE RE
  | Star RE
  | Capture RE
  | Backref Int
  | Sup RE RE
  | Neg RE
  deriving (Show, Eq)


-- data RE = CharClass CharSet | Seq [RE] | Alt RE RE | Star RE | Capture RE | Backref Int | Sup RE RE | Neg RE deriving (Show, Eq)

-- cChar:: CharSet
-- cChar = S.fromList "erikalaj"




-- cChar:: Parser CharSet
-- cChar = fmap S.fromList asd

-- asd :: Parser String
-- asd = char '"' *> (many (noneOf ['"'])) <* char '"'