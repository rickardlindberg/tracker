module Tracking.Persistence where

import LogTime
import Text.ParserCombinators.Parsec
import Tracking

trackingFromFile :: FilePath -> IO Tracking
trackingFromFile = fmap parseTracking . readFile

trackingToFile :: FilePath -> Tracking -> IO ()
trackingToFile path = writeFile path . formatTracking

formatTracking :: Tracking -> String
formatTracking t = header ++ logs
    where
        header  = name t ++ "\n"
        logs    = concatMap entry (entries t)
        entry e = formatLogTime (time e) ++ " | " ++ show (value e) ++ " | " ++ comment e ++ "\n"

parseTracking :: String -> Tracking
parseTracking str =
    case parse file "" str of
        Left  _ -> error "baaaah"
        Right x -> x
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
