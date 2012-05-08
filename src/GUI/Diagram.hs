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
    let points = map (\entry -> (entry, toX $ time entry, toY $ value entry)) (sortedEntries tracking)
    let closest = findClosestEntry points mousePos
    -- Line between points
    setSourceRGBA 0 0 0 0.5
    setLineWidth 1
    forM_ points $ \(entry, px, py) ->
        lineTo px py
    stroke
    -- Points
    setSourceRGB 0 0 0
    let r = 3
    forM_ points $ \(entry, px, py) -> do
        arc px py r 0 (2*pi)
        fill
    -- Highlight
    forM_ points $ \(entry, px, py) ->
        when (entry == closest) $ do
            -- Extra focus ring
            arc px py (r + 2) 0 (2*pi)
            stroke
            -- Balloon
            renderBalloon [show $ time entry, show $ value entry, comment entry] w h px py

renderBalloon :: [String] -> Double -> Double -> Double -> Double -> Render ()
renderBalloon text w h px py = do
    let innerBorder       = 5
    let bubbleBorderSpace = 5
    let arrowHeight       = 15
    -- Coord text
    (tw, th) <- textBounds text
    let ydiff = (th/2 + arrowHeight) * if py > h/2 then (-1) else 1

    let bubbleIdealInner = move 0 ydiff $ move (-tw/2) (-th/2) $ Rect px py tw th
    let bubbleOuter = shrink bubbleBorderSpace $ ensureTopLeftVisible w h $ expand (innerBorder+bubbleBorderSpace) bubbleIdealInner
    let bubbleInner = shrink innerBorder bubbleOuter

    bubble bubbleOuter px py
    poss <- textPositions text bubbleInner
    setSourceRGB 0 0 0
    forM_ poss $ \(line, x, y) -> do
        moveTo x y
        showText line

ensureTopLeftVisible :: Double -> Double -> Rect -> Rect
ensureTopLeftVisible tw th rect@(Rect x y w h) = move dx dy rect
    where
        dx  | x < 0      = -x
            | (x+w) > tw = tw-(x+w)
            | otherwise  = 0
        dy  | y < 0      = -y
            | (y+h) > th = th-(y+h)
            | otherwise  = 0

textBounds :: [String] -> Render (Double, Double)
textBounds text = do
    extents     <- mapM textExtents text
    let widths  =  map textExtentsWidth  extents
    let heights =  map textExtentsHeight extents
    return (maximum widths, (maximum heights + 2) * fromIntegral (length text))

textPositions :: [String] -> Rect -> Render [(String, Double, Double)]
textPositions text (Rect x y w h) = do
    let deltaY = h / fromIntegral (length text)
    let indices = [0..(length text - 1)]
    forM (zip text indices) $ \(line, i) -> do
        extent <- textExtents line
        return (line,
                x - textExtentsXbearing extent,
                y - textExtentsYbearing extent + deltaY * fromIntegral i)

bubble :: Rect -> Double -> Double -> Render ()
bubble (Rect x y w h) px py = do
    let boxRadius = 5
    let bubblePath = do
        newPath
        when (py < y) $ do
            lineTo (px-boxRadius) y
            lineTo px py
            lineTo (px+boxRadius) y
            return ()
        arc (x+w-boxRadius) (y+  boxRadius) boxRadius ((-90) * pi/180) (0   * pi/180)
        arc (x+w-boxRadius) (y+h-boxRadius) boxRadius (0     * pi/180) (90  * pi/180)
        when (py > y) $ do
            lineTo (px+boxRadius) (y+h)
            lineTo px py
            lineTo (px-boxRadius) (y+h)
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
