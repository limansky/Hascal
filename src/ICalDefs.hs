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


isoTimeFormat = "%Y%m%dT%H%M%SZ"
isoTimeFormatDate = "%Y%m%d"
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

data DateTime = Date { getDay :: Day }
    | DateTime { getUTCTime :: UTCTime }
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

makeSimpleDateTimeProperty f _ s = f $ parseTime defaultTimeLocale isoTimeFormat s

makeDateTimeProperty f ps s = f d
                            where d = if (calDateValue, calDateValueDate) `elem` ps
                                        then parseTime defaultTimeLocale isoTimeFormatDate s >>= \date -> Just . Date $ utctDay date
                                        else parseTime defaultTimeLocale isoTimeFormat s >>= \date -> Just $ DateTime date
