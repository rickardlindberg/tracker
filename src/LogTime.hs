module LogTime
    ( getCurrentLogTime
    , parseLogTime
    , formatLogTime
    , logTimePercent
    ) where

import Data.Time
import System.Locale

getCurrentLogTime :: IO UTCTime
getCurrentLogTime = fmap (truncToMinutePrecision . localTimeToUTC utc) getCurrentLocalTime

truncToMinutePrecision :: UTCTime -> UTCTime
truncToMinutePrecision = parseLogTime . formatLogTime

getCurrentLocalTime :: IO LocalTime
getCurrentLocalTime = do
    localZone <- getCurrentTimeZone
    nowUtc    <- getCurrentTime
    return    $  utcToLocalTime localZone nowUtc

format :: String
format = "%Y-%m-%d %H:%M"

formatLogTime :: UTCTime -> String
formatLogTime = formatTime defaultTimeLocale format

parseLogTime :: String -> UTCTime
parseLogTime = readTime defaultTimeLocale format

logTimePercent :: UTCTime -> UTCTime -> UTCTime -> Double
logTimePercent min max t = realToFrac $ (t `diffUTCTime` min)  / (max `diffUTCTime` min)
