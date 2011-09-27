module ICalDefs where

calBegin = "BEGIN"
calEnd = "END"
calVevent = "VEVENT"
calDtStart = "DTSTART"
calDtEnd = "DTEND"
calSummary = "SUMMARY"
calDescription = "DESCRIPTION"
calUid = "UID"

isoTimeFormat = "%Y%m%dT%H%M%SZ"
crlf = "\r\n"
colon = ':'
semicolon = ';'
dquote = '"'
comma = ','

data ComponentProperty = Uid { propertyUid :: String }
   deriving (Eq, Show)

makeSimpleProperty f _ s = f s
