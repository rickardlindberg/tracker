module Main (main) where

import Graphics.UI.Gtk
import MainWindow

main :: IO ()
main = do
    initGUI
    showMainWindow
    mainGUI
