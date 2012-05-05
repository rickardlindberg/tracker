import Data.IORef
import Data.Time.Clock
import Diagram
import Graphics.UI.Gtk
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
    t <- myCurrentTime
    trackingRef   <- newIORef $ Tracking "foo" [ TrackingEntry (myParseTime "2012-05-05 00:00") 1 ""
                                               , TrackingEntry (myParseTime "2012-05-05 01:00") 2 ""
                                               , TrackingEntry (myParseTime "2012-05-05 02:00") 1 ""
                                               , TrackingEntry (myParseTime "2012-05-05 03:00") 2 ""
                                               , TrackingEntry t 1.5 ""
                                               ]
    initDiagramComponent canvas (readIORef trackingRef)
    widgetShowAll mainWindow
    windowResize mainWindow 500 300
    return ()
