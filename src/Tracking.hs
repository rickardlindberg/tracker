module Tracking where

import GHC.Exts
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

sortedEntries :: Tracking -> [TrackingEntry]
sortedEntries tracking = sortWith time $ entries tracking

addLog :: Tracking -> TrackingEntry -> Tracking
addLog tracking entry = tracking { entries = newEntries }
    where
        newEntries = entry : entries tracking

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
