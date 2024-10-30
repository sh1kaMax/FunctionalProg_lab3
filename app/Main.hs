{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Main (main) where

import Control.Concurrent (MVar, forkIO, newEmptyMVar, newMVar, putMVar, takeMVar, withMVar)
import Interpolation (Message, lagranInterpolation, linearInterpolation)
import System.Environment

printCoords :: [Message] -> MVar () -> IO ()
printCoords [] _ = putStrLn ""
printCoords points@((_, msg) : xs) printLock
  | msg == "end" || msg == "" = printCoords xs printLock
  | otherwise = do
      withMVar printLock $ \_ -> do
        let printHelp :: [Message] -> IO ()
            printHelp [] = return ()
            printHelp ((point, msg1) : xs1)
              | msg1 == "end" = print point
              | otherwise = do
                  putStr msg1
                  print point
                  printHelp xs1
        printHelp points
      printCoords xs printLock

main :: IO ()
main = do
  args <- getArgs
  let [method, start, step] = map read args :: [Double]
  let methods = [[linearInterpolation], [lagranInterpolation], [linearInterpolation, lagranInterpolation]] !! round method
  pointsList <- fmap (map words . lines) getContents
  let points = map (\[x, y] -> (read x, read y)) pointsList :: [(Double, Double)]
  printLock <- newMVar ()

  case methods of
    [singleMethod] ->
      printCoords (singleMethod points step start) printLock
    [method1, method2] -> do
      done1 <- newEmptyMVar
      done2 <- newEmptyMVar

      forkIO $ do
        printCoords (method1 points step start) printLock
        putMVar done1 ()

      forkIO $ do
        printCoords (method2 points step start) printLock
        putMVar done2 ()

      takeMVar done1
      takeMVar done2
      putStrLn "End of the program"
