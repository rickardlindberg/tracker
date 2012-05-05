module MainWindow (showMainWindow) where

import Data.IORef
import Diagram
import Graphics.UI.Gtk
import Paths_tracker
import Tracking

showMainWindow :: IO ()
showMainWindow = do
    trackingRef   <- newIORef $ Tracking "foo" []
    builder       <- getDataFileName "interface.glade" >>= builderFromFile
    mainWindow    <- builderGetObject builder castToWindow            "main_window"
    canvas        <- builderGetObject builder castToDrawingArea       "canvas"
    mainWindow    `onDestroy`         mainQuit
    initDiagramComponent canvas (readIORef trackingRef)
    widgetShowAll mainWindow

builderFromFile :: FilePath -> IO Builder
builderFromFile path = do
    builder <- builderNew
    builderAddFromFile builder path
    return builder
