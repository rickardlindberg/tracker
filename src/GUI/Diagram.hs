module GUI.Diagram where

import Control.Monad
import Data.IORef
import Geometry.Rect
import GHC.Exts
import Graphics.Rendering.Cairo hiding (status, Status)
import Graphics.UI.Gtk
import Tracking

initDiagramComponent :: DrawingArea -> IO Tracking -> IO ()
initDiagramComponent canvas readTrackings = do
    (readMousePos, writeMousePos) <- createRef (0, 0)
    widgetAddEvents canvas [PointerMotionMask]
    canvas `onExpose` redraw readTrackings readMousePos canvas
    canvas `on`       motionNotifyEvent $ tryEvent $ do
                          mousePos <- eventCoordinates
                          liftIO $ do
                              writeMousePos mousePos
                              postGUIAsync $ widgetQueueDraw canvas
    return ()

createRef :: a -> IO (IO a, a -> IO ())
createRef value = newIORef value >>= \ref -> return (readIORef ref, writeIORef ref)

redraw readTrackings readMousePos canvas event = do
    tracking   <- readTrackings
    mousePos   <- readMousePos
    (w, h)     <- widgetGetSize canvas
    let render =  renderScreen tracking mousePos (fromIntegral w) (fromIntegral h)
    drawing    <- widgetGetDrawWindow canvas
    renderWithDrawable drawing render
    return True

renderScreen :: Tracking -> (Double, Double) -> Double -> Double -> Render ()
renderScreen tracking mousePos w h = do
    let border = 20
    let (Rect ox y tw th) = shrink border (Rect 0 0 w h)
    let oy = y + th

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
            let text = show entry
            ex <- textExtents text
            let tw = textExtentsWidth ex
            let th = textExtentsHeight ex
            let innerBorder = 5
            let ydiff = th * 2 * if py > h/2 then (-1) else 1
            bubble (expand innerBorder $ move (-tw/2) (-th/2+ydiff) $ Rect px py tw th) px py
            moveTo (px - textExtentsXbearing ex - tw/2) (py - textExtentsYbearing ex + ydiff - innerBorder)
            setSourceRGB 0 0 0
            showText text

bubble :: Rect -> Double -> Double -> Render ()
bubble (Rect x y w h) px py = do
    let boxRadius = 5
    let bubblePath = do
        newPath
        when (py < y) $ do
            lineTo (x+w/2-boxRadius) y
            lineTo (x+w/2) py
            lineTo (x+w/2+boxRadius) y
            return ()
        arc (x+w-boxRadius) (y+  boxRadius) boxRadius ((-90) * pi/180) (0   * pi/180)
        arc (x+w-boxRadius) (y+h-boxRadius) boxRadius (0     * pi/180) (90  * pi/180)
        when (py > y) $ do
            lineTo (x+w/2+boxRadius) (y+h)
            lineTo (x+w/2) py
            lineTo (x+w/2-boxRadius) (y+h)
            return ()
        arc (x+  boxRadius) (y+h-boxRadius) boxRadius (90    * pi/180) (180 * pi/180)
        arc (x+  boxRadius) (y+  boxRadius) boxRadius (180   * pi/180) (270 * pi/180)
        closePath
    -- Background
    bubblePath
    setSourceRGBA 1 1 1 0.8
    fill
    -- Border
    bubblePath
    setSourceRGBA 0 0 0 0.4
    setLineWidth 1
    stroke

findClosestEntry :: [(TrackingEntry, Double, Double)] -> (Double, Double) -> TrackingEntry
findClosestEntry points (mx, my) = fst $ myMin $ map (\(e, x, y) -> (e, dist (mx, my) (x, y))) points
    where
        dist (x1, y1) (x2, y2) = sqrt $ (x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2)
        myMin = head . sortWith snd
