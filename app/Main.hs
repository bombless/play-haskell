module Main where

import Main.Status (runStatus)
import Main.QueryFont (queryFont)
import Main.Strings (test)

main :: IO ()
main = do
    queryFont
    runStatus
    test
