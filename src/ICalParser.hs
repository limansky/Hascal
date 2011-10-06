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
tSafeChar = noneOf ([semicolon, comma, backslash] ++ crlf) <|> escapedChar

escapedChar :: CharParser () Char
escapedChar = do
    char backslash
    ((char 'n' <|> char 'N') >> return '\n') 
        <|> oneOf [backslash, semicolon, comma]

simpleStringLine s = do
    v <- string s
    eol
    return v

property name f vp = do
    string name
    p <- many propertyParam
    char colon
    v <- vp
    eol
    return $ f p v

textProperty name f = property name f propertyValueText
generalProperty name f = property name f propertyValueGeneral

unknownProperty = do
#if MIN_VERSION_parsec(3,0,0)
    notFollowedBy . string $ calEnd ++ [colon]
#else
    notFollowedBy $ string (calEnd ++ [colon]) >> return 'a'
#endif
    name <- propertyName
    p <- many propertyParam
    char colon
    v <- propertyValueGeneral
    eol
    return $ makeUnknownProperty name p v

propertyName = many1 safeChar
propertyValueText = many1 tSafeChar
propertyValueGeneral = many1 $ noneOf crlf

propertyParam = do
    char semicolon
    n <- paramName
    char '='
    v <- paramValue
    return (n, v)

paramName = many1 $ noneOf "="

paramValue = quotedString <|> many1 safeChar

uid = textProperty calUid $ makeStringProperty Uid
summary = generalProperty calSummary $ makeStringProperty Summary
description = generalProperty calDescription $ makeStringProperty Description
organizer = generalProperty calOrganizer makeOrganizer
attendee = generalProperty calAttendee makeAttendee
location = generalProperty calLocation $ makeStringProperty Location
priority = generalProperty calPriority $ makeNumProperty Priority
dateStamp = generalProperty calDateStamp $ makeSimpleDateTimeProperty DateStamp
dateStart = generalProperty calDateStart $ makeDateTimeProperty DateStart
dateEnd = generalProperty calDateEnd $ makeDateTimeProperty DateEnd

icalFile = many1 calItem

calItem = vevent

vevent = between veventBegin veventEnd veventContent

veventBegin = simpleStringLine $ calBegin ++ [colon] ++ calVevent
veventEnd = simpleStringLine $ calEnd ++ [colon] ++ calVevent

veventContent = do
    ps <- many $ tryChoice [ uid
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
    return $ EventData ps

version = generalProperty calVersion $ makeStringProperty Version
propId = generalProperty calProdId $ makeStringProperty ProdId

vcalendar = between vcalendarBegin vcalendarEnd vcalendarContent

vcalendarBegin = simpleStringLine $ calBegin ++ [colon] ++ calVcalendar
vcalendarEnd = simpleStringLine $ calEnd ++ [colon] ++ calVcalendar

vcalendarContent = many $ tryChoice [ version
                                    , propId
                                    , vevent
                                    ]

parseIcal s = parse vcalendar "Invalid data" $ unfoldIcal s
