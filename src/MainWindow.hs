module MainWindow (showMainWindow) where

import Graphics.UI.Gtk
import Paths_tracker

showMainWindow :: IO ()
showMainWindow = do
    builder       <- getDataFileName "interface.glade" >>= builderFromFile
    mainWindow    <- builderGetObject builder castToWindow            "main_window"
    mainWindow    `onDestroy`         mainQuit
    widgetShowAll mainWindow

builderFromFile :: FilePath -> IO Builder
builderFromFile path = do
    builder <- builderNew
    builderAddFromFile builder path
    return builder
