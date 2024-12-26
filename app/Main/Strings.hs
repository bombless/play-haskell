module Main.Strings (test) where

import System.Random (randomRIO)
import Control.Monad (replicateM)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

-- 生成一个随机字符串，长度在 1 到 100 之间
randomString :: IO String
randomString = do
    len <- randomRIO (1, 100)  -- 随机字符串长度
    replicateM len (randomRIO ('a', 'z'))  -- 生成随机字符并组合成字符串

-- 生成 n 个随机字符串
generateStrings :: Int -> IO [String]
generateStrings n = replicateM n randomString

test :: IO ()
test = do
    putStrLn "开始生成随机字符串..."
    start <- getCurrentTime  -- 获取开始时间
    strings <- generateStrings 1000000  -- 生成一百万个字符串
    end <- getCurrentTime  -- 获取结束时间
    putStrLn $ "生成完成，共生成 " ++ show (length strings) ++ " 个字符串。"
    putStrLn $ "耗时: " ++ show (diffUTCTime end start)
