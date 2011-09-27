module ICalParser
{-    (
        parseIcal
    ) -}
where

import ICalDefs

import Text.ParserCombinators.Parsec
import Data.Map (fromList)

unfoldIcal [] = []
unfoldIcal ('\r':'\n':' ':xs) = unfoldIcal xs
unfoldIcal ('\r':'\n':'\t':xs) = unfoldIcal xs
unfoldIcal (x:xs) = x:(unfoldIcal xs)

-- See rfc5545 3.1
eol = string crlf
quotedString = between (char dquote) (char dquote) (many qSafeChar)
safeChar = noneOf [dquote, colon, semicolon, comma]
qSafeChar = noneOf [dquote]

simpleStringLine s = do
    v <- string s
    eol
    return v

property name f = do
    string name
    p <- many propertyParam
    char colon
    v <- propertyValue
    eol
    return $ f p v

propertyParam = do
    char semicolon
    n <- paramName
    char '='
    v <- paramValue
    return (n, v)

paramName = many1 $ noneOf "="

paramValue = quotedString <|> many1 safeChar

uid = property calUid $ makeSimpleProperty Uid

icalFile = many1 calItem

calItem = vevent

vevent = between veventBegin veventEnd veventContent

veventBegin = simpleStringLine $ calBegin ++ [colon] ++ calVevent
veventEnd = simpleStringLine $ calEnd ++ [colon] ++ calVevent

veventContent = undefined

propertyValue = many $ noneOf crlf

parseIcal s = undefined --fmap fromList $ parse icalFile "Invalid data" s
