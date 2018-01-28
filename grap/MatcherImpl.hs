module MatcherImpl where

import qualified Data.Set as S
import AST
import Control.Monad

-- Generic string-matching monad. Do not change anything in
-- the following definitions.

newtype Matcher d a =
  Matcher {runMatcher :: String -> Int -> d -> [(Int, d, a)]}

instance Monad (Matcher d) where
  return v = Matcher (\s i d -> return (0, d, v))
  m >>= f =
    Matcher (\s i d -> do (j, d', a) <- runMatcher m s i d
                          (j', d'', b) <- runMatcher (f a) s (i + j) d'
                          return (j + j', d'', b))
  fail s = Matcher (\s i d -> [])

instance Functor (Matcher d) where fmap = liftM
instance Applicative (Matcher d) where pure = return; (<*>) = ap

-- Associated functions to implement. Their definitions may freely use
-- the Matcher term constructor.

nextChar :: Matcher d Char
nextChar = undefined

getData :: Matcher d d
getData = undefined

modifyData :: (d -> d) -> Matcher d ()
modifyData = undefined

protectData :: Matcher d a -> Matcher d a
protectData = undefined

pick :: [Matcher d a] -> Matcher d a
pick = undefined

grab :: Matcher d a -> Matcher d (String, a)
grab = undefined

both :: Matcher d a -> (a -> Matcher d b) -> Matcher d b
both = undefined

neg :: Matcher d a -> Matcher d ()
neg = undefined

-- Grap-specific functions to implement. Should not use the Matcher
-- term constructor, only the above functions.

type Captures = [String]

match :: RE -> Matcher Captures ()
match = undefined

matchTop :: RE -> String -> Maybe Captures
matchTop = undefined
