module GUI.LogEntryDialog where

import Control.Monad
import Graphics.UI.Gtk
import LogTime
import Tracking
import Tracking.Persistence

data LogEntryDialog = LogEntryDialog
    { dialog       :: Dialog
    , valueSpin    :: Entry
    , commentEntry :: Entry
    }

initLogEntryDialog :: LogEntryDialog -> (TrackingEntry -> IO ()) -> IO (IO ())
initLogEntryDialog x saveLog = do
    (valueSpin x `onEditableChanged` validateNumber x)
    let runDialog = do
        beforeDialogOpened x
        response <- dialogRun (dialog x)
        when (response == ResponseOk) $ do
            time    <- getCurrentLogTime
            value   <- fmap read (entryGetText (valueSpin x))
            comment <- entryGetText (commentEntry x)
            saveLog (TrackingEntry time value comment)
        widgetHide (dialog x)
    return runDialog

validateNumber :: LogEntryDialog -> IO ()
validateNumber x = do
    text <- entryGetText (valueSpin x)
    case parseMaybeDouble text of
        Just d -> widgetRestoreBase (valueSpin x) StateNormal
        _      -> widgetModifyBase  (valueSpin x) StateNormal (Color 65535 35535 35535)

beforeDialogOpened :: LogEntryDialog -> IO ()
beforeDialogOpened x = do
    entrySetText (valueSpin x) ""
    entrySetText (commentEntry x) ""
    widgetGrabFocus (valueSpin x)
