module Event where

import Data.Time

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
