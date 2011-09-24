module ICalParser
    (
        parseIcal
    )
where

import Text.ParserCombinators.Parsec
import Data.Map (fromList)

-- See rfc5545 3.1
eol = string "\r\n"

icalFile = endBy line eol

line = do
    k <- key
    char ':'
    v <- value
    return (k, v)

key = many (noneOf ":\r\n")
value = many (noneOf "\r\n")

parseIcal s = fmap fromList $ parse icalFile "Invalid data" s
