module Tracking.Persistence where

import LogTime
import Tracking

trackingFromFile :: FilePath -> IO Tracking
trackingFromFile = fmap parseTracking . readFile

trackingToFile :: FilePath -> Tracking -> IO ()
trackingToFile path = writeFile path . formatTracking

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
