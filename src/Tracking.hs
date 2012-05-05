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

timePercent :: Tracking -> LogTime -> Double
timePercent tracking t = logTimePercent min max t
    where
        max = maximum $ map time (entries tracking)
        min = minimum $ map time (entries tracking)

valuePercent :: Tracking -> Double -> Double
valuePercent tracking v = (v - min)  / (max - min)
    where
        max = maximum $ map value (entries tracking)
        min = minimum $ map value (entries tracking)

formatTracking :: Tracking -> String
formatTracking t = header ++ rest
    where
        header  = name t ++ "\n"
        rest    = concat $ concatMap entry (entries t)
        entry e = (formatLogTime (time e) ++ " -> " ++ show (value e) ++ "\n") : xxx (comment e)
        xxx     = map (\e -> "  " ++ e ++ "\n") . lines

parseTracking :: String -> Tracking
parseTracking str =
    let (header:rest)   = lines str
        toPairs [] = []
        toPairs (x:y:r) = (x, y):toPairs r
        entries         = map foo (toPairs rest)
        foo (x, y)      = let [d,t,"->",v] = words x
                              (' ':' ':comment) = y
                              time = parseLogTime (d ++ " " ++ t)
                              value = read v
                          in TrackingEntry time value comment
    in Tracking header entries
