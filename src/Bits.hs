--
-- Author: Ivan
-- File: Bits.hs
-- Data: 2013-03-24


module Bits where

import qualified AST as A
import Data.Char(ord,toLower,toUpper)
import Control.Applicative

type Result a = Either String a

data Bit = H | L | X | Z
    deriving(Eq,Ord,Read,Enum)

instance Show Bit where
    show b = "1b'" ++ (c b)
        where
        c H = "1"
        c L = "0"
        c X = "x"
        c Z = "z"
        

enum_bits = [H .. Z]

-- digits {{{
digit_to_bits (A.DecDigit d) = Right $ int_to_bits (read d)
digit_to_bits (A.BinDigit d) = read_bin_digit d
digit_to_bits (A.BinIxDigit d) = read_bin_digit d
digit_to_bits _              = Left $ "Hex digits can't support"


zipWithDefault ::  (a -> a -> b) -> a -> [a] -> [a] -> [b]
zipWithDefault f def [] []         = []
zipWithDefault f def [] (y:ys)     = (f def y) : zipWithDefault f def [] ys
zipWithDefault f def (x:xs) []     = (f x def) : zipWithDefault f def xs []
zipWithDefault f def (x:xs) (y:ys) = (f x y)   : zipWithDefault f def xs ys

read_bin_digit :: String -> Result [Bit]
read_bin_digit s = do
    let len = length value
    let default_value = L
    let prefix = replicate (range - len) default_value
    result <- mapM (\c -> to_either c $ lookup c table) value
    return $ (prefix ++ result)
    where
    (range,value) = split_digit s
    to_either _ (Just x) = Right x
    to_either v Nothing  = Left $ "Can't find digit symbol for value '" ++ v : "'"
    table = [('0',L)
        ,('1',H)
        ,('x',X)
        ,('z',Z)]

split_digit :: String -> (Int, String)
split_digit s = (read prefix, value )
    where
    (prefix,val) = span (/= '\'') s
    value = tail $ map toLower $ tail val

--- }}}
-- {{{bits

int_to_bits ::  Int -> [Bit]
int_to_bits value = reverse $ map to_bit $ go value
    where
    to_bit 0 = L
    to_bit 1 = H
    
    go 0 = [0]
    go 1 = [1]
    go v = let i = v `div` 2
               r = v `mod` 2
            in r : go ((v - r) `div` 2)


bit_bool H = Just True
bit_bool L = Just False
bit_bool _ = Nothing

bits_to_int bits = snd $ foldr (\v (p,r) ->  (p*2, r + (to_int v) * p) ) (1,0) bits
    where
    to_int v = case bit_bool v of
                    Just True -> 1
                    Just False -> 0
                    Nothing -> error $ "Undefined int value for `" ++ (show v) ++ "` in Eval.hs"

-- }}}


