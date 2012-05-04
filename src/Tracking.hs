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

timePercent :: Tracking -> Int -> Double
timePercent tracking t = (fromIntegral t - min)  / (max - min)
    where
        max = fromIntegral $ maximum $ map time (entries tracking)
        min = fromIntegral $ minimum $ map time (entries tracking)

valuePercent :: Tracking -> Double -> Double
valuePercent tracking v = (v - min)  / (max - min)
    where
        max = maximum $ map value (entries tracking)
        min = minimum $ map value (entries tracking)
