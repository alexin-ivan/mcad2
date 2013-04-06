--
-- Author: Ivan
-- File: TestSyntax.hs
-- Data: 2013-02-16


module Main where

import Syntax
import Lex
import Tokens

import System.Environment
import AST(print_ast)


test :: IO ()
test = do
    return ()


main = do
    args <- getArgs
    let fn = args!!0
    s <- readFile fn
    let toks = alexScanTokens fn s
    let in_file = "In File \"" ++ fn ++ "\" " 
    case toks of
        Left s -> putStrLn $ in_file ++ s
        Right ts -> do
                        writeFile (fn ++ ".lex") (show ts)
                        let ast = analyze_toks ts
                        case ast of
                            Left s -> putStrLn $ in_file ++ (findError s ts)
                            Right x -> do
								writeFile (fn ++ ".ast") (show x)
								writeFile (fn ++ ".ast.v") (print_ast x)


