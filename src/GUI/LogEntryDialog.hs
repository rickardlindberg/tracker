module GUI.LogEntryDialog where

import Control.Monad
import Graphics.UI.Gtk
import LogTime
import Tracking

data LogEntryDialog = LogEntryDialog
    { dialog       :: Dialog
    , valueSpin    :: Entry
    , commentEntry :: Entry
    }

initLogEntryDialog :: LogEntryDialog -> (TrackingEntry -> IO ()) -> IO (IO ())
initLogEntryDialog x saveLog = do
    let runDialog = do
        response <- dialogRun (dialog x)
        when (response == ResponseOk) $ do
            time    <- getCurrentLogTime
            value   <- fmap read (entryGetText (valueSpin x))
            comment <- entryGetText (commentEntry x)
            saveLog (TrackingEntry time value comment)
        widgetHide (dialog x)
    return runDialog
