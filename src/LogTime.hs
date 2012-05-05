module LogTime
    ( LogTime()
    , getCurrentLogTime
    , parseLogTime
    , formatLogTime
    , logTimePercent
    ) where

import Data.Time
import System.Locale

newtype LogTime = LogTime UTCTime deriving (Eq, Ord)
extract (LogTime t) = t
wrap = LogTime
instance Show LogTime where
    show = formatLogTime

getCurrentLogTime :: IO LogTime
getCurrentLogTime = fmap (wrap . truncToMinutePrecision . localTimeToUTC utc) getCurrentLocalTime

truncToMinutePrecision :: UTCTime -> UTCTime
truncToMinutePrecision = extract . parseLogTime . formatLogTime . wrap

getCurrentLocalTime :: IO LocalTime
getCurrentLocalTime = do
    localZone <- getCurrentTimeZone
    nowUtc    <- getCurrentTime
    return    $  utcToLocalTime localZone nowUtc

format :: String
format = "%Y-%m-%d %H:%M"

formatLogTime :: LogTime -> String
formatLogTime = formatTime defaultTimeLocale format . extract

parseLogTime :: String -> LogTime
parseLogTime = wrap . readTime defaultTimeLocale format

logTimePercent :: LogTime -> LogTime -> LogTime -> Double
logTimePercent min max t =
    realToFrac $ (extract t   `diffUTCTime` extract min) /
                 (extract max `diffUTCTime` extract min)
