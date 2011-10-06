{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

module ICalParserTests where

import ICalParser
import ICalDefs

import Data.Char (isPunctuation)
import Text.ParserCombinators.Parsec (parse)
import Test.QuickCheck

(===) :: Eq a => Either t a -> a -> Bool
(Right a) === b = a == b
_ === _ = False

newtype NonEscapedNonEmptyText a = NonEscapedNonEmptyText [a]
    deriving (Eq, Ord, Show, Read)

instance Arbitrary Char => Arbitrary (NonEscapedNonEmptyText Char) where
    arbitrary = NonEscapedNonEmptyText `fmap` (arbitrary `suchThat` isNonEscapedNonEmptyText)
    shrink (NonEscapedNonEmptyText xs) = [ NonEscapedNonEmptyText xs' | xs' <- shrink xs, isNonEscapedNonEmptyText xs' ]

isNonEscapedNonEmptyText xs = not (null xs) && isNonEscapedText xs

-- see rfc5545 - Section 3.3.11.  Text
isNonEscapedText xs = all isTextChar xs && isNonEscaped xs
    where isTextChar a = a == ' ' || -- SPACE
                         a == '\t' || -- HTAB
                         a >= '\x21' && a <= '\x7e' || -- including ':' and '"' and escaped ';', '\\', ','
                         a > '\x7f' -- NON-US-ASCII
          isNonEscaped [] = True
          isNonEscaped ('\\':x:xs) = not (isEscapedChar x) && isNonEscaped xs
          isNonEscaped (_:xs) = isNonEscaped xs

isEscapedChar a = a == '\\' || a == ';' || a == ',' || a == 'N' || a == 'n'

prop_uid (NonEscapedNonEmptyText s) = parse uid "" ("UID:" ++ escape s ++ "\r\n") === Uid s

escape [] = []
escape (x:xs)
    | isEscapedChar x && isPunctuation x = '\\' : x : escape xs
    | otherwise = x : escape xs
