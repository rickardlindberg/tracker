import Data.IORef
import Data.Time.Clock
import Graphics.UI.Gtk
import GUI.Diagram
import LogTime
import System
import Tracking

main :: IO ()
main = do
    initGUI
    showMainWindow
    mainGUI

showMainWindow :: IO ()
showMainWindow = do
    mainWindow <- windowNew
    canvas <- drawingAreaNew
    set mainWindow [ windowTitle := "Diagram Test App", containerChild := canvas ]
    mainWindow    `onDestroy`         mainQuit
    t <- getCurrentLogTime
    trackingRef   <- newIORef $ Tracking "foo" [ TrackingEntry (parseLogTime "2012-05-05 00:00") 1 ""
                                               , TrackingEntry (parseLogTime "2012-05-05 01:00") 2 ""
                                               , TrackingEntry (parseLogTime "2012-05-05 02:00") 1 "a comment here"
                                               , TrackingEntry (parseLogTime "2012-05-05 03:00") 2 ""
                                               , TrackingEntry t 1.5 ""
                                               ]
    initDiagramComponent canvas (readIORef trackingRef)
    widgetShowAll mainWindow
    windowResize mainWindow 500 300
    return ()
