{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Lexer

numToTokens :: Int -> [Token]
numToTokens x = if x >= 0 then [Val x] else [Sub, Val (abs x)]

strToToken :: String -> Token -- Doesn't work cuz when testing strings like '%' '+' ... they fail.
strToToken var = Var (trim var)
    where trim = takeWhile (/= ' ') . dropWhile (== ' ')

prop_Val :: Int -> Bool
prop_Val x = getTokens (show x) == numToTokens x

prop_Var :: String -> Bool -- Doesn't work cuz when testing strings like '%' '+' ... they fail.
prop_Var str = getTokens ("var" ++ str) == [strToToken ("var" ++ str)]

prop_Add :: Int -> Int -> Bool
prop_Add x y = getTokens (show x ++ "+" ++ show y) == numToTokens x ++ [Add] ++ numToTokens y

prop_Ass :: String -> Int -> Bool
prop_Ass str x = getTokens (("var" ++ str) ++ " := " ++ show x) == strToToken ("var" ++ str) : Ass : numToTokens x
    where var = takeWhile (/= ' ') "var" ++ str

prop_Cond :: Int -> Bool
prop_Cond x =  getTokens ("if" ++ show x) == Cond : numToTokens x

prop_Div :: Int -> Int -> Bool
prop_Div x y = getTokens (show x ++ "/" ++ show y) == numToTokens x ++ [Div] ++ numToTokens y

prop_Loop :: Int -> Bool
prop_Loop x =  getTokens ("while" ++ show x) == Loop : numToTokens x

prop_Lth :: Int -> Int -> Bool
prop_Lth x y = getTokens (show x ++ "<" ++ show y) == numToTokens x ++ [Lth] ++ numToTokens y

prop_Mod :: Int -> Int -> Bool
prop_Mod x y = getTokens (show x ++ "%" ++ show y) == numToTokens x ++ [Mod] ++ numToTokens y

prop_Mul :: Int -> Int -> Bool
prop_Mul x y = getTokens (show x ++ "*" ++ show y) == numToTokens x ++ [Mul] ++ numToTokens y

prop_Neq :: Int -> Int -> Bool
prop_Neq x y = getTokens (show x ++ "!=" ++ show y) == numToTokens x ++ [Neq] ++ numToTokens y

prop_Sub :: Int -> Int -> Bool
prop_Sub x y = getTokens (show x ++ "-" ++ show y) == numToTokens x ++ [Sub] ++ numToTokens y

return []
runTests = $quickCheckAll

main :: IO Bool
main = runTests
