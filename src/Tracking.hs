module Tracking where

import Data.Time
import LogTime

data Tracking = Tracking
    { name    :: String
    , entries :: [TrackingEntry]
    } deriving (Show, Eq)

data TrackingEntry = TrackingEntry
    { time    :: UTCTime
    , value   :: Double
    , comment :: String
    } deriving (Show, Eq)

timePercent :: Tracking -> UTCTime -> Double
timePercent tracking t = logTimePercent min max t
    where
        max = maximum $ map time (entries tracking)
        min = minimum $ map time (entries tracking)

valuePercent :: Tracking -> Double -> Double
valuePercent tracking v = (v - min)  / (max - min)
    where
        max = maximum $ map value (entries tracking)
        min = minimum $ map value (entries tracking)
