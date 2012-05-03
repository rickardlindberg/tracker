module Tracking where

data Tracking = Tracking
    { name    :: String
    , entries :: [TrackingEntry]
    } deriving (Show, Eq)

data TrackingEntry = TrackingEntry
    { time    :: Int
    , value   :: Double
    , comment :: String
    } deriving (Show, Eq)

minTime :: Tracking -> Int
minTime t = minimum $ map time (entries t)

maxTime :: Tracking -> Int
maxTime t = maximum $ map time (entries t)

minValue :: Tracking -> Double
minValue t = minimum $ map value (entries t)

maxValue :: Tracking -> Double
maxValue t = maximum $ map value (entries t)
