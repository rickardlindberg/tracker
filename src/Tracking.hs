module Tracking where

import Data.Time
import System.Locale

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
timePercent tracking t = realToFrac $ (t `diffUTCTime` min)  / (max `diffUTCTime` min)
    where
        max = maximum $ map time (entries tracking)
        min = minimum $ map time (entries tracking)

valuePercent :: Tracking -> Double -> Double
valuePercent tracking v = (v - min)  / (max - min)
    where
        max = maximum $ map value (entries tracking)
        min = minimum $ map value (entries tracking)

myCurrentTime :: IO UTCTime
myCurrentTime = do
    localZone <- getCurrentTimeZone
    nowUtc    <- getCurrentTime
    let localInUtc = localTimeToUTC utc . utcToLocalTime localZone
    let reduceToMinuteResolution = myParseTime . myFormatTime
    return $ reduceToMinuteResolution $ localInUtc nowUtc

format :: String
format = "%Y-%m-%d %H:%M"

myFormatTime :: UTCTime -> String
myFormatTime = formatTime defaultTimeLocale format

myParseTime :: String -> UTCTime
myParseTime = readTime defaultTimeLocale format
