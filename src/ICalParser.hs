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

paramValue = many1 $ noneOf [colon, semicolon]

icalFile = many1 calItem

calItem = vevent

vevent = between veventBegin veventEnd veventContent

veventBegin = simpleStringLine $ calBegin ++ [colon] ++ calVevent
veventEnd = simpleStringLine $ calEnd ++ [colon] ++ calVevent

veventContent = undefined

propertyValue = many $ noneOf crlf

parseIcal s = undefined --fmap fromList $ parse icalFile "Invalid data" s
