module MainWindow (showMainWindow) where

import Data.IORef
import Graphics.Rendering.Cairo hiding (status, Status)
import Graphics.UI.Gtk
import Paths_tracker
import Tracking

showMainWindow :: IO ()
showMainWindow = do
    trackingRef   <- newIORef $ Tracking "foo" [ TrackingEntry 1 1 ""
                                               , TrackingEntry 2 2 ""
                                               ]
    builder       <- getDataFileName "interface.glade" >>= builderFromFile
    mainWindow    <- builderGetObject builder castToWindow            "main_window"
    canvas        <- builderGetObject builder castToDrawingArea       "canvas"
    mainWindow    `onDestroy`         mainQuit
    canvas        `onExpose`          redraw canvas trackingRef
    widgetShowAll mainWindow

builderFromFile :: FilePath -> IO Builder
builderFromFile path = do
    builder <- builderNew
    builderAddFromFile builder path
    return builder

redraw canvas trackingRef event = do
    (w, h) <- widgetGetSize canvas
    drawin <- widgetGetDrawWindow canvas
    tracking <- readIORef trackingRef
    renderWithDrawable drawin (renderScreen tracking (fromIntegral w) (fromIntegral h))
    return True

renderScreen :: Tracking -> Double -> Double -> Render ()
renderScreen tracking w h = do
    setSourceRGB 1 1 0.6
    paint
