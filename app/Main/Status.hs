module Main.Status (runStatus) where

import Control.Concurrent (threadDelay)
import System.Random (randomRIO)

-- 模拟用户输入查询函数 (返回 IO Bool)
queryInput :: IO Bool
queryInput = do
    -- 这里用随机数模拟用户输入
    randomValue <- randomRIO (0, 1) :: IO Int  -- 随机生成 0 或 1
    return (randomValue == 1)        -- 只有随机值为 1 时返回 True

-- 模拟更新数字的函数 (接收一个整数并返回一个新的随机数)
updateNumber :: Int -> IO Int
updateNumber prev = do
    newNumber <- randomRIO (1, 100)  -- 生成 1 到 100 之间的随机数
    return $ newNumber + prev          -- 返回新数字加上上一个数字

-- 主函数
runStatus :: IO ()
runStatus = do
    let loop :: Int -> IO ()
        loop prev = do
            -- 每三秒查询一次用户输入
            threadDelay 3000000  -- 3秒，单位是微秒

            -- 获取 queryInput 的结果
            result <- queryInput
            if result then do
                -- 如果返回 True，调用 updateNumber 并输出结果
                newNumber <- updateNumber prev  -- 传入上一个数字
                putStrLn $ "New random number: " ++ show newNumber
                -- 递归调用 loop，传递新的数字作为上一个值
                loop newNumber
            else do
                -- 如果返回 False，不更新，继续循环
                putStrLn "No update, queryInput returned False"
                loop prev  -- 传递原来的值，继续循环

    -- 启动循环，初始值为 0
    loop 0