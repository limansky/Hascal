{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

module ICalParserTests where

import ICalParser
import ICalDefs

import Data.Char (isPunctuation)
import Text.ParserCombinators.Parsec (parse)
import Test.QuickCheck
import Test.HUnit

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

escape [] = []
escape (x:xs)
    | isEscapedChar x && isPunctuation x = '\\' : x : escape xs
    | otherwise = x : escape xs

(@?==) :: (Show a, Show a1, Eq a) => Either a1 a -> a -> Assertion
(Right a) @?== b = a == b @? "expected: " ++ (show b) ++ "\n but got: " ++ (show a) ++ "\n"
(Left err) @?== _ = assertFailure $ show err

(@?!) :: Either t t1 -> [Char] -> Assertion
(Left _) @?! _ = return ()
_ @?! s = assertFailure $ "fail expected but passed\n Message: " ++ s

-- uid parser tests
prop_uid (NonEscapedNonEmptyText s) = parse uid "" ("UID:" ++ escape s ++ "\r\n") === Uid s

test_uid_escaped_value = parse uid "" "UID:1: v1\\, v2\\;\\n2: v3\\N3:\r\n" @?== Uid "1: v1, v2;\n2: v3\n3:"

test_uid_empty = parse uid "" "UID:\r\n" @?! "UID can't be empty"
