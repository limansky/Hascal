module Event where

import Data.Time
import System.Locale

data Duration = Interval NominalDiffTime | AllDay deriving (Eq)

data Event = Event {
      eventId :: Int
    , eventTitle :: String
    , eventDescription :: String
    , eventStart :: UTCTime
    , eventDuration :: Duration
    , eventReminder :: Maybe NominalDiffTime
    } 
    deriving (Eq)

instance Show Event where
    show e = show id ++ " : " ++ show t ++ " (" ++ period ++ ") rem: " ++ show r
        where id = eventId e
              t = eventTitle e
              r = eventReminder e
              s = eventStart e
              period = case eventDuration e of
                 AllDay     -> show . utctDay $ s
                 Interval i -> show s ++ " - " ++ (show $ addUTCTime i s)

calBegin = "BEGIN"
calEnd = "END"
calVevent = "VEVENT"
calDtStart = "DTSTART"
calDtEnd = "DTEND"
calSummary = "SUMMARY"
calDescription = "DESCRIPTION"
isoTimeFormat = "%Y%m%dT%H%M%SZ"
crlf = "\r\n"

calLine key value = key ++ ": " ++ value ++ crlf

eventToICal e = calLine calBegin calVevent
    ++ calLine calDtStart (formatTime defaultTimeLocale isoTimeFormat $ eventStart e)
    ++ endTimeLine
    ++ calLine calSummary (eventTitle e)
    ++ calLine calDescription (head desc)
    ++ descFooter
    ++ calLine calEnd calVevent
        where endTimeLine = case eventDuration e of
                AllDay -> ""
                Interval i -> let endTime = addUTCTime i (eventStart e)
                              in calLine calDtEnd (formatTime defaultTimeLocale isoTimeFormat endTime)
              desc = lines . eventDescription $ e
              descFooter = concat $ map (\s -> " " ++ s ++ crlf) (tail desc)
