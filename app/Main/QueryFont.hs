module Main.QueryFont (queryFont) where


-- import Control.Monad (filterM)
import Text.Printf (printf)

import System.Directory (listDirectory)
import System.FilePath ((</>))
-- import System.FilePath (takeExtension)

import Foreign.C.String (withCString)
import Foreign.Ptr (nullPtr)

import SDL.Raw.Font (fontFaces, closeFont)
import qualified SDL.Raw.Font as Raw (openFont, init, quit)

getFontFiles :: IO [FilePath]
getFontFiles = do
    let fontDir = "C:\\Windows\\Fonts"
    allFiles <- listDirectory fontDir
    print allFiles
    -- 筛选出字体文件（例如：.ttf, .otf）
    -- filterM isFontFile (map (fontDir </>) allFiles)
    return $ map (fontDir </>) allFiles

-- isFontFile :: FilePath -> IO Bool
-- isFontFile path = do
--     let ext = takeExtension path
--     if ext == ".ttf" || ext == ".otf"
--         then return True
--         else do
--             printf "This filename is %s, not so good!\n" path
--             return False

queryFont :: IO ()
queryFont = do
    let printEveryLine :: FilePath -> IO ()
        printEveryLine file = do
            print file
            numFaces file >>= printf "faces count %d\n"
    _ <- Raw.init
    mapM_ printEveryLine =<< getFontFiles
    Raw.quit

numFaces :: FilePath -> IO Int
numFaces path = do
    font <- withCString path (`Raw.openFont` 12)
    if font == nullPtr
        then do
            putStrLn "Raw.openFont failed"
            return 0
        else do
            num <- fontFaces font
            closeFont font
            return $ fromIntegral num
            

