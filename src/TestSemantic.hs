--
-- Author: Ivan
-- File: TestSemantic.hs
-- Data: 2013-03-16


module Main where

import TestGetAST
import Semantic3
import Debug.Trace

test :: IO ()
test = do
    r <-  get_ast
    case r of
        Just (fn,ast) -> putStrLn $ do_test $ trace ("ReadFile: " ++ fn) ast
        Nothing -> return ()
    return ()

main :: IO ()
main = do
    test
    return ()
