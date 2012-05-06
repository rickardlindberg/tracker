module GUI where

import Graphics.UI.Gtk
import MainWindow

guiMain :: FilePath -> IO ()
guiMain trackingPath = do
    initGUI
    showMainWindow
    mainGUI
