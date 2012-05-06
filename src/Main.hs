module Main (main) where

import GUI
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of
        [trackingPath] -> guiMain trackingPath
        _              -> putStrLn "usage: tracking path-to-tracking"
