module Main where

import Main.Status (runStatus)
import Main.QueryFont (queryFont)
import Main.Strings (test)
import Main.Gui (run)

main :: IO ()
main = do
    queryFont
    runStatus
    test
    run
