module Regexp where

import Prelude hiding (seq)

data Regexp = Empty
            | Epsilon
            | Char Char
            | Seq Regexp Regexp
            | Alt Regexp Regexp
            | Star Regexp
            deriving (Show, Eq, Ord)

match :: Regexp -> String -> Bool
match r s = nullable (foldl (flip derivative) r s)

derivative :: Char -> Regexp -> Regexp
derivative a Empty = Empty
derivative a Epsilon = Empty
derivative a (Char b) | a /= b = Empty
                      | otherwise = Epsilon
derivative a (Seq r1 r2) | nullable r1 = Alt (Seq (derivative a r1) r2) (derivative a r2)
                         | otherwise = Seq (derivative a r1) r2
derivative a (Alt r1 r2) = Alt (derivative a r1) (derivative a r2)
derivative a sr@(Star r) = Seq (derivative a r) sr

nullable :: Regexp -> Bool
nullable Empty = False
nullable Epsilon = True
nullable (Char a) = False
nullable (Seq r1 r2) = (nullable r1) && (nullable r2)
nullable (Alt r1 r2) = (nullable r1) || (nullable r2)
nullable (Star r)= True