module ICalDefs where

import Data.Time
import System.Locale

calBegin = "BEGIN"
calEnd = "END"
calVevent = "VEVENT"
calDtStart = "DTSTART"
calDtEnd = "DTEND"
calSummary = "SUMMARY"
calDescription = "DESCRIPTION"
calUid = "UID"

calCN = "CN"
calRole = "ROLE"
calPartStat = "PARTSTAT"
calOrganizer = "ORGANIZER"
calAttendee = "ATTENDEE"
calLocation = "LOCATION"
calPriority = "PRIORITY"

calRoleChair = "CHAIR"
calRoleReq = "REQ-PARTICIPANT"
calRoleOpt = "OPT-PARTICIPANT"
calRoleNon = "NON-PARTICIPANT"

calPartStatNeedsAction = "NEEDS-ACTION"
calPartStatAccepted = "ACCEPTED"
calPartStatDeclined = "DECLINED"
calPartStatTentative = "TENTATIVE"
calPartStatDelegated = "DELEGATED"
calPartStatCompleted = "COMPLETED"
calPartStatInProgress = "IN-PROCESS"

calDateStamp = "DTSTAMP"
calDateStart = "DTSTART"
calDateEnd = "DTEND"
calDateValue = "VALUE"
calDateValueDate = "DATE"
calTimeZoneId = "TZID"


timeFormatUtcDateTime = "%Y%m%dT%H%M%SZ"
timeFormatLocalDateTime = "%Y%m%dT%H%M%S"
timeFormatZonedDateTime = "%Y%m%dT%H%M%S%Z"
timeFormatDate = "%Y%m%d"

crlf = "\r\n"
colon = ':'
semicolon = ';'
dquote = '"'
comma = ','

-- Looks like custom roles can be also
data Role = Chair | ReqParticipant | OptParticipant | NonParticipant
    deriving (Eq, Show)

data PartStat = NeedsAction | Accepted | Declined | Tentative | Delegated | Completed | InProgress
    deriving (Eq, Show)

-- for now we will use hardcoded  current timezone
-- we will get it from state  or something then
currentTimezone = TimeZone 240 False "MSK"

-- fir now we will use hardcoded map for long-to-short
-- timezone names. We will use some real map then
mapTimeZoneNameAbbr = [
      ("Europe/Moscow", "MSK")
    ]

instance Eq ZonedTime where
    a == b = (zonedTimeToLocalTime a) == (zonedTimeToLocalTime b) && (zonedTimeZone a) == (zonedTimeZone b)

data DateTime = Date { getDay :: Day }
    | UTCDateTime { getUTCTime :: UTCTime }
    | LocalDateTime { getLocalTime :: LocalTime }
    | ZonedDateTime { getZonedTime :: ZonedTime }
    deriving (Eq, Show)

data ComponentProperty = Uid { propertyUid :: String }
    | Summary { propertySummary :: String }
    | Description { propertyDescription :: String }
    | Location { propertyLocation :: String }
    | Priority { propertyPriority :: Integer }
    | Organizer { propertyOrganizer :: String
                , organizerName :: Maybe String }
    | Attendee { propertyAttendee :: String
               , attendeeName :: Maybe String
               , attendeeRole :: Maybe Role
               , attendeePartStat :: Maybe PartStat }
    | DateStamp { propertyDateStamp :: Maybe UTCTime }
    | DateStart { propertyDateStart :: Maybe DateTime }
    | DateEnd { propertyDateStart :: Maybe DateTime }
    | Unknown { propertyUnknownName :: String
              , propertyUnknownValue :: String
              , propertyUnknownParams :: [(String, String)] }
    deriving (Eq, Show)

makeStringProperty f _ s = f s

makeNumProperty f _ s = f $ read s

makeOrganizer p s = Organizer s name
    where name = lookup calCN p

roles = [ (calRoleChair, Chair)
        , (calRoleReq, ReqParticipant)
        , (calRoleOpt, OptParticipant)
        , (calRoleNon, NonParticipant)
        ]

partStats = [ (calPartStatNeedsAction, NeedsAction)
            , (calPartStatAccepted, Accepted)
            , (calPartStatDeclined, Declined)
            , (calPartStatTentative, Tentative)
            , (calPartStatDelegated, Delegated)
            , (calPartStatCompleted, Completed)
            , (calPartStatInProgress, InProgress)
            ]

makeAttendee p s = Attendee s name role partStat
    where name = lookup calCN p
          role = lookup calRole p >>= (`lookup` roles)
          partStat = lookup calPartStat p >>= (`lookup` partStats)

makeSimpleDateTimeProperty f _ s = f $ parseTime defaultTimeLocale timeFormatUtcDateTime s

makeDateTimeProperty f ps s = f $ day `chain` zonedtime `chain` utctime `chain` localtime
                            where chain Nothing fu = fu
                                  chain just _ = just
                                  day = if (calDateValue, calDateValueDate) `elem` ps
                                          then parseTime defaultTimeLocale timeFormatDate s >>= \date -> Just . Date $ utctDay date
                                          else Nothing
                                  zonedtime = lookup calTimeZoneId ps >>= (`lookup` mapTimeZoneNameAbbr) >>= Just . (s ++) >>= parseTime defaultTimeLocale timeFormatZonedDateTime >>= \date -> Just $ ZonedDateTime date
                                  utctime =  parseTime defaultTimeLocale timeFormatUtcDateTime s >>= \date -> Just $ UTCDateTime date
                                  localtime = unlookup calDateValue ps >> unlookup calTimeZoneId ps >> parseTime defaultTimeLocale timeFormatLocalDateTime s >>= \date -> Just $ LocalDateTime date
                                  unlookup k s = if (lookup k s) == Nothing
                                                   then Just True
                                                   else Nothing

makeUnknownProperty n p v = Unknown n v p
