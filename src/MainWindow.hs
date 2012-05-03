module MainWindow (showMainWindow) where

import Control.Monad
import Data.IORef
import Graphics.Rendering.Cairo hiding (status, Status)
import Graphics.UI.Gtk
import Paths_tracker
import Tracking

showMainWindow :: IO ()
showMainWindow = do
    trackingRef   <- newIORef $ Tracking "foo" [ TrackingEntry 1 1 ""
                                               , TrackingEntry 2 2 ""
                                               , TrackingEntry 3 1 ""
                                               , TrackingEntry 4 2 ""
                                               , TrackingEntry 5 1.5 ""
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
    let border = 20
    let (ox, oy) = (border, h - border)
    let tw = w - 2 * border
    let th = h - 2 * border

    -- Background
    setSourceRGB 1 1 0.6
    paint

    -- Axises
    setSourceRGB 0 0 0
    setLineWidth 2
    -- X-axis
    moveTo ox oy
    lineTo (ox+tw) oy
    stroke
    -- Y-axis
    moveTo ox oy
    lineTo ox (oy-th)
    stroke

    -- Data points
    let vMinTime  = fromIntegral $ minTime tracking
    let vMaxTime  = fromIntegral $ maxTime tracking
    let vMinValue = minValue tracking
    let vMaxValue = maxValue tracking
    let toX time  = ox + tw * (fromIntegral time - vMinTime)  / (vMaxTime  - vMinTime)
    let toY value = oy - th * (value             - vMinValue) / (vMaxValue - vMinValue)
    -- Line between points
    setSourceRGBA 0 0 0 0.5
    setLineWidth 1
    forM_ (entries tracking) $ \entry -> do
        let px = toX (time entry)
        let py = toY (value entry)
        lineTo px py
    stroke
    -- Points
    setSourceRGB 0 0 0
    forM_ (entries tracking) $ \entry -> do
        let px = toX (time entry)
        let py = toY (value entry)
        let r = 3
        arc px py r 0 (2*pi)
        fill
