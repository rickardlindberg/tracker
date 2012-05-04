module Diagram where

import Control.Monad
import Data.IORef
import GHC.Exts
import Graphics.Rendering.Cairo hiding (status, Status)
import Graphics.UI.Gtk
import Tracking

initDiagram canvas getTrackings = do
    mousePosRef <- newIORef (0, 0)
    let forceRedraw = postGUIAsync $ widgetQueueDraw canvas
    widgetAddEvents canvas [PointerMotionMask]
    canvas        `onExpose`          redraw canvas getTrackings mousePosRef
    canvas        `on`                motionNotifyEvent $ tryEvent $ do
                                          (x, y) <- eventCoordinates
                                          liftIO $ writeIORef mousePosRef (x, y) >> forceRedraw

redraw canvas getTrackings mousePosRef event = do
    (w, h) <- widgetGetSize canvas
    drawin <- widgetGetDrawWindow canvas
    tracking <- getTrackings
    mousePos <- readIORef mousePosRef
    renderWithDrawable drawin (renderScreen tracking mousePos (fromIntegral w) (fromIntegral h))
    return True

renderScreen :: Tracking -> (Double, Double) -> Double -> Double -> Render ()
renderScreen tracking mousePos w h = do
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
    let toX time  = ox + tw * timePercent tracking time
    let toY value = oy - th * valuePercent tracking value
    let points = map (\entry -> (entry, toX $ time entry, toY $ value entry)) (entries tracking)
    let closest = findClosestEntry points mousePos
    -- Line between points
    setSourceRGBA 0 0 0 0.5
    setLineWidth 1
    forM_ points $ \(entry, px, py) ->
        lineTo px py
    stroke
    -- Points
    setSourceRGB 0 0 0
    forM_ points $ \(entry, px, py) -> do
        let r = 3
        arc px py r 0 (2*pi)
        fill
        -- Balloon
        when (entry == closest) $ do
            arc px py (r + 2) 0 (2*pi)
            stroke
            -- Coord text
            moveTo 20 20
            setSourceRGB 0 0 0
            showText (show entry)

findClosestEntry :: [(TrackingEntry, Double, Double)] -> (Double, Double) -> TrackingEntry
findClosestEntry points (mx, my) = fst $ myMin $ map (\(e, x, y) -> (e, dist (mx, my) (x, y))) points
    where
        dist (x1, y1) (x2, y2) = sqrt $ (x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2)
        myMin = head . sortWith snd
