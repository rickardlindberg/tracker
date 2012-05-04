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
    trackingRef   <- newIORef $ Tracking "foo" [ TrackingEntry 1 1 ""
                                               , TrackingEntry 2 2 ""
                                               , TrackingEntry 3 1 ""
                                               , TrackingEntry 4 2 ""
                                               , TrackingEntry 5 1.5 ""
                                               ]
    initDiagram canvas trackingRef
    widgetShowAll mainWindow
    windowResize mainWindow 500 300
    return ()
