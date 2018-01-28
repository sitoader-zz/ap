import Text.ParserCombinators.Parsec hiding ((<|>), many, option)
import Text.ParserCombinators.Parsec.Combinator
import Control.Applicative
import Control.Monad
import Data.Char ( isSpace)
import qualified Data.Set as S  
import Debug.Trace (trace)

import AST


-- data RE =
--     CharClass CharSet
--   | Seq [RE]
--   | Alt RE RE
--   | Star RE
--   | Capture RE
--   | Backref Int
--   | Sup RE RE
--   | Neg RE
--   deriving (Show, Eq) stringLiteral = bracketOpen *> (many (noneOf ['"'])) <* bracketClose

-- | a Parser for symbols
-- symP :: Parser Char
-- symP = liftM CharSet $ satisfy (\x -> and (map (x /=) [')', '^', '$', '*', '+', '|', '?']))


fullSet::GenParser Char st String
fullSet = do
                char '['
                full <- charSet
                return full;

charSet::GenParser Char st String
charSet = do
                first <- try (getRange) <|> try (getOne) <|> string "." <|> string "]"
                if first == "]" then
                        return []
                else
                  do 
                        later <- charSet
                        return (first ++ later)

getOne::GenParser Char st String
getOne = do
                some <- oneOf  ['a'..'z'] <|> oneOf  ['A'..'Z'] <|> oneOf  ['0'..'9']
                return [some]

getRange::GenParser Char st String
getRange = getRangeaz  <|> getRangeAZ  <|> getRange09

getRangeaz::GenParser Char st String
getRangeaz = do
    c1 <- oneOf ['a'..'z']
    _ <- char '-'
    c2 <- oneOf ['a'..'z']
    return [c1..c2]

getRangeAZ::GenParser Char st String
getRangeAZ = do
    c1 <- oneOf ['A'..'Z']
    _ <- char '-'
    c2 <- oneOf ['A'..'Z']
    return [c1..c2] 

getRange09::GenParser Char st String
getRange09 = do
    c1 <- oneOf ['0'..'9']
    _ <- char '-'
    c2 <- oneOf ['0'..'9']
    return [c1..c2] 

charClassParser::Parser RE
charClassParser = CharClass <$> (fmap S.fromList $ fullSet)


-------------


altParser::Parser RE
altParser = do
    s1 <- choice [supParser, charClassParser,negParser]
    _  <- char '|'
    s2 <- choice [supParser,charClassParser,negParser]
    return (Alt s1 s2)


---------------
negRE::Parser String
negRE = do
    char '('
    x <- many (noneOf ['!', '&', '(', ')', '*', '+', '.', '?', '[', '\\', '{', '|', '}'])
    char ')'
    char '!'
    return x

negParser::Parser RE
negParser = Neg <$> (CharClass <$> (fmap S.fromList $ negRE))

-------------


supParser::Parser RE
supParser = do
    s1 <- choice [charClassParser,negParser,altParser]
    _  <- char '&'
    s2 <- choice [charClassParser,negParser,altParser]
    return (Sup s1 s2)

----------

reParser::Parser RE
reParser = charClassParser
-- supParser <|> altParser <|>  charClassParser  <|> negParser

-- -- | a parser for general statements
-- statementP :: GenParser Char Reg
-- statementP = sequenceP <|> nonSequenceP where
--   altP = do
--     s1 <- choice [charClassParser]
--     _  <- char '|'
--     s2 <- parensP
--     return (Alt s1 s2)
--   repP = do
--     s1 <- parensP
--     _  <- char '*'
--     return (Rep s1)
--   zeroP = do
--     s1 <- parensP
--     _  <- char '?'
--     return (ZeroOrOne s1)
--   oneP = do
--     s1 <- parensP
--     _  <- char '+'
--     return (Seq s1 (Rep s1))
--   sequenceP = do
--     s1 <- nonSequenceP
--     s2 <- statementP
--     return (Seq s1 s2)
--   nonSequenceP = choice [charClassParser,negParser,altParser]













bracketOpen :: Parser Char
bracketOpen = char '['

bracketClose :: Parser Char
bracketClose = char ']'

