module LogEntryDialog where

import Control.Monad
import Data.IORef
import Graphics.UI.Gtk
import LogTime
import Tracking

data LogEntryDialog = LogEntryDialog
    { dialog       :: Dialog
    , valueSpin    :: Entry
    , commentEntry :: Entry
    }

initLogEntryDialog :: LogEntryDialog -> IORef Tracking -> IO (IO ())
initLogEntryDialog x trackingRef = do
    let runDialog = do
        response <- dialogRun (dialog x)
        when (response == ResponseOk) $ do
            time    <- getCurrentLogTime
            value   <- fmap read (entryGetText (valueSpin x))
            comment <- entryGetText (commentEntry x)
            modifyIORef trackingRef (\t -> addLog t time value comment)
        widgetHide (dialog x)
    return runDialog
