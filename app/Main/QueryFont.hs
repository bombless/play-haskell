module Main.QueryFont (queryFont) where


-- import Control.Monad (filterM)
import Text.Printf (printf)

import System.Directory (listDirectory)
import System.FilePath ((</>))
-- import System.FilePath (takeExtension)

import Foreign.C.String (withCString)
import Foreign.Ptr (nullPtr)

import SDL.Raw.Font (fontFaces, closeFont)
import qualified SDL.Raw.Font as Raw (openFont, openFontIndex, init, quit, glyphIsProvided)
import Control.Monad (when, unless)
import Foreign.C.Types (CLong)
import GHC.Base (ord)

getFontFiles :: IO [FilePath]
getFontFiles = do
    let fontDir = "C:\\Windows\\Fonts"
    allFiles <- listDirectory fontDir
    -- 筛选出字体文件（例如：.ttf, .otf）
    -- filterM isFontFile (map (fontDir </>) allFiles)
    return $ map (fontDir </>) allFiles


queryFont :: IO ()
queryFont = do
    let printEveryLine :: FilePath -> IO ()
        printEveryLine file = do
            num <- numFaces file
            when (num > 1) $ do
                printf "% 5d %s\n" num file
                let showEveryFace :: CLong -> IO ()
                    showEveryFace i = do
                        font <- withCString file (\p -> Raw.openFontIndex p 12 i)
                        printsA <- Raw.glyphIsProvided font (fromIntegral $ ord 'A')
                        closeFont font
                        unless (printsA == 0) $ printf "% 5d is good\n" (fromIntegral i :: Int)
                mapM_ showEveryFace [0 .. fromIntegral num - 1]
    _ <- Raw.init
    fontFiles <- getFontFiles  -- 获取字体文件
    mapM_ printEveryLine fontFiles  -- 执行每个文件的操作
    Raw.quit  -- 退出 Raw

numFaces :: FilePath -> IO Int
numFaces path = do
    font <- withCString path (`Raw.openFont` 12)
    if font == nullPtr
        then do
            printf "Raw.openFont failed: %s\n" path
            return 0
        else do
            num <- fontFaces font
            closeFont font
            return $ fromIntegral num
            

