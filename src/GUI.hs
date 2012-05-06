module GUI where

import Graphics.UI.Gtk
import GUI.MainWindow

guiMain :: FilePath -> IO ()
guiMain trackingPath = do
    initGUI
    showMainWindow
    mainGUI
