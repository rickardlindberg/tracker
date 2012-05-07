module Tracking where

import LogTime

data Tracking = Tracking
    { name    :: String
    , entries :: [TrackingEntry]
    } deriving (Show, Eq)

data TrackingEntry = TrackingEntry
    { time    :: LogTime
    , value   :: Double
    , comment :: String
    } deriving (Show, Eq)

addLog :: Tracking -> LogTime -> Double -> String -> Tracking
addLog tracking time value comment = tracking { entries = newEntries }
    where
        newEntries = TrackingEntry time value comment : entries tracking

timePercent :: Tracking -> LogTime -> Double
timePercent tracking t = logTimePercent min max t
    where
        max = maximum $ map time (entries tracking)
        min = minimum $ map time (entries tracking)

valuePercent :: Tracking -> Double -> Double
valuePercent tracking v = if (max - min) == 0 then 1 else (v - min)  / (max - min)
    where
        max = maximum $ map value (entries tracking)
        min = minimum $ map value (entries tracking)
