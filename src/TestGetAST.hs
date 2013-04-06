--
-- Author: Ivan
-- File: TestGetAST.hs
-- Data: 2013-02-22


module TestGetAST where

import Syntax
import Lex
import Tokens

import System.Environment

get_ast = do
    args <- getArgs
    let fn = args!!0
    s <- readFile fn
    let toks = alexScanTokens fn s
    let in_file = "In File \"" ++ fn ++ "\" " 
    case toks of
        Left s -> do
                    putStrLn $ in_file ++ s
                    return Nothing
        Right ts -> do
                        -- writeFile (fn ++ ".lex") (show ts)
                        let ast = analyze_toks ts
                        case ast of
                            Left s -> do
                                         putStrLn $ in_file ++ (findError s ts)
                                         return Nothing
                            Right x -> do
                                         return $ Just (fn,x)


