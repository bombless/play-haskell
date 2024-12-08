module Main.QueryFont (queryFont) where


import Control.Monad (filterM)
import Text.Printf (printf)

import System.Directory (listDirectory)
import System.FilePath ((</>), takeExtension)

getFontFiles :: IO [FilePath]
getFontFiles = do
    let fontDir = "C:\\Windows\\Fonts"
    allFiles <- listDirectory fontDir
    -- 筛选出字体文件（例如：.ttf, .otf）
    filterM isFontFile (map (fontDir </>) allFiles)

isFontFile :: FilePath -> IO Bool
isFontFile path = do
    let ext = takeExtension path
    if ext == ".ttf" || ext == ".otf"
        then return True
        else do
            printf "This filename is %s, not so good!\n" path
            return False

queryFont :: IO ()
queryFont = do
    let printEveryLine :: FilePath -> IO ()
        printEveryLine = (>> putStrLn "This looks good.") . print
    mapM_ printEveryLine =<< getFontFiles

