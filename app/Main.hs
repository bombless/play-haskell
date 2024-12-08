module Main where

import Main.Status (runStatus)
import Main.QueryFont (queryFont)

main :: IO ()
main = do
    queryFont
    runStatus
