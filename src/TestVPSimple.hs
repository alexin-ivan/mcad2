--
-- Author: Ivan
-- File: TestVPSimple.hs
-- Data: 2013-04-07


module Main where

import System.Environment
import VPSimple

main :: IO ()
main = do
    args <- getArgs
    case args of
        (x:[]) -> parse x >>= putStrLn
        (x:y:[]) -> translate (read x)  (read y)
        [] -> return ()


