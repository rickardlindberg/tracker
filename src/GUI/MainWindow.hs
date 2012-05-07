module GUI.MainWindow (showMainWindow) where

import Data.IORef
import Graphics.UI.Gtk
import GUI.Diagram
import GUI.LogEntryDialog
import Paths_tracker
import Tracking

showMainWindow :: FilePath -> IO ()
showMainWindow trackingPath = do
    tracking      <- fmap parseTracking (readFile trackingPath)
    trackingRef   <- newIORef tracking
    builder       <- getDataFileName "interface.glade" >>= builderFromFile
    mainWindow    <- builderGetObject builder castToWindow            "main_window"
    canvas        <- builderGetObject builder castToDrawingArea       "canvas"
    mainWindow    `onDestroy`         mainQuit
    initDiagramComponent canvas (readIORef trackingRef)

    d <- builderGetObject builder castToDialog "logEntryDialog"
    v <- builderGetObject builder castToEntry "logEntryValue"
    c <- builderGetObject builder castToEntry "logEntryComment"
    runLogEntryDialog <- initLogEntryDialog LogEntryDialog { dialog = d
                                                           , valueSpin = v
                                                           , commentEntry = c
                                                           }
                                            trackingRef

    addEntryButton <- builderGetObject builder castToButton "addEntryButton"
    addEntryButton `onClicked` runLogEntryDialog

    widgetShowAll mainWindow

builderFromFile :: FilePath -> IO Builder
builderFromFile path = do
    builder <- builderNew
    builderAddFromFile builder path
    return builder