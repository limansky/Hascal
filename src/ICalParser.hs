{-# LANGUAGE CPP #-}

module ICalParser
{-    (
        parseIcal
    ) -}
where

import ICalDefs

import Text.ParserCombinators.Parsec

unfoldIcal [] = []
unfoldIcal ('\r':'\n':' ':xs) = unfoldIcal xs
unfoldIcal ('\r':'\n':'\t':xs) = unfoldIcal xs
unfoldIcal (x:xs) = x:(unfoldIcal xs)

tryChoice = choice . map try

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

unknownProperty = do
-- #if MIN_VERSION_parsec(3,0,0)
    notFollowedBy . string $ calEnd ++ [colon]
-- #else
--    notFollowedBy $ string (calEnd ++ [colon]) >> return 'a'
-- #endif
    name <- propertyName
    p <- many propertyParam
    char colon
    v <- propertyValue
    eol
    return $ makeUnknownProperty name p v

propertyName = many1 safeChar
propertyValue = many $ noneOf crlf

propertyParam = do
    char semicolon
    n <- paramName
    char '='
    v <- paramValue
    return (n, v)

paramName = many1 $ noneOf "="

paramValue = quotedString <|> many1 safeChar

uid = property calUid $ makeStringProperty Uid
summary = property calSummary $ makeStringProperty Summary
description = property calDescription $ makeStringProperty Description
organizer = property calOrganizer makeOrganizer
attendee = property calAttendee makeAttendee
location = property calLocation $ makeStringProperty Location
priority = property calPriority $ makeNumProperty Priority
dateStamp = property calDateStamp $ makeSimpleDateTimeProperty DateStamp
dateStart = property calDateStart $ makeDateTimeProperty DateStart
dateEnd = property calDateEnd $ makeDateTimeProperty DateEnd

icalFile = many1 calItem

calItem = vevent

vevent = between veventBegin veventEnd veventContent

veventBegin = simpleStringLine $ calBegin ++ [colon] ++ calVevent
veventEnd = simpleStringLine $ calEnd ++ [colon] ++ calVevent

veventContent = many $ tryChoice [ uid
                                 , summary
                                 , description
                                 , organizer
                                 , attendee
                                 , location
                                 , priority
                                 , dateStamp
                                 , dateStart
                                 , dateEnd
                                 , unknownProperty
                                 ]

parseIcal s = undefined --fmap fromList $ parse icalFile "Invalid data" s
