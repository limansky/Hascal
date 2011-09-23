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
    show (Event id t _ s (Interval i) r) = show id ++ " : " ++ show t ++ " (" ++ show s ++ " - " ++ (show $ addUTCTime i s) ++ ") rem: " ++ show r
    show (Event id t _ s AllDay r)       = show id ++ " : " ++ show t ++ " (" ++ show (utctDay s) ++ ") rem: " ++ show r
