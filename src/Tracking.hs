module Tracking where

data Tracking = Tracking
    { name    :: String
    , entries :: [TrackingEntry]
    } deriving (Show, Eq)

data TrackingEntry = TrackingEntry
    { value   :: Double
    , time    :: Int
    , comment :: String
    } deriving (Show, Eq)
