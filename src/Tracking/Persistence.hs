module Tracking.Persistence where

import LogTime
import Text.ParserCombinators.Parsec
import Tracking

trackingFromFile :: FilePath -> IO (Maybe Tracking)
trackingFromFile = fmap parseTracking . readFile

trackingToFile :: FilePath -> Tracking -> IO ()
trackingToFile path = writeFile path . formatTracking

formatTracking :: Tracking -> String
formatTracking t = fHeader ++ fLogs
    where
        fHeader  = name t ++ newline
        fLogs    = concatMap fEntry (entries t)
        fEntry e = formatLogTime (time e) ++ sep ++ show (value e) ++ sep ++ comment e ++ newline
        sep      = " | "
        newline  = "\n"

parseTracking :: String -> Maybe Tracking
parseTracking str =
    case parse file "" str of
        Left  _ -> Nothing
        Right x -> Just x
    where
        file = do
            header <- restOfLine
            logs   <- many log
            eof
            return $ Tracking header logs
        log = do
            time    <- fmap parseLogTime (count 16 anyChar)
            sep
            value   <- fmap read nonSpace
            sep
            comment <- restOfLine
            return  $  TrackingEntry time value comment
        sep = string " | "
        restOfLine = do
            rest <- many (noneOf "\n")
            newline
            return rest
        nonSpace = many (noneOf " ")
