{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Lexer

num_to_tokens :: Int -> [Token]
num_to_tokens x = if (x >= 0) then [Val x] else [Sub, Val (abs x)]

str_to_token :: String -> Token -- Doesn't work cuz when testing strings like '%' '+' ... they fail.
str_to_token var = Var (trim var)
    where trim = takeWhile (\c -> c /= ' ') . dropWhile (\c -> c == ' ')

prop_Val :: Int -> Bool
prop_Val x = get_tokens (show x) == num_to_tokens x

prop_Var :: String -> Bool -- Doesn't work cuz when testing strings like '%' '+' ... they fail.
prop_Var str = get_tokens ("var" ++ str) == [str_to_token ("var" ++ str)]

prop_Add :: Int -> Int -> Bool
prop_Add x y = get_tokens ((show x) ++ "+" ++ (show y)) == (num_to_tokens x) ++ [Add] ++ (num_to_tokens y)

prop_Ass :: String -> Int -> Bool
prop_Ass str x = get_tokens (("var" ++ str) ++ " := " ++ (show x)) == str_to_token ("var" ++ str) : [Ass] ++ num_to_tokens x
    where var = takeWhile (\c -> c /= ' ') "var" ++ str

prop_Cond :: Int -> Bool
prop_Cond x =  get_tokens ("if" ++ show x) == [Cond] ++ num_to_tokens x

prop_Div :: Int -> Int -> Bool
prop_Div x y = get_tokens ((show x) ++ "/" ++ (show y)) == (num_to_tokens x) ++ [Div] ++ (num_to_tokens y)

prop_Loop :: Int -> Bool
prop_Loop x =  get_tokens ("while" ++ show x) == [Loop] ++ num_to_tokens x

prop_Lth :: Int -> Int -> Bool
prop_Lth x y = get_tokens ((show x) ++ "<" ++ (show y)) == (num_to_tokens x) ++ [Lth] ++ (num_to_tokens y)

prop_Mod :: Int -> Int -> Bool
prop_Mod x y = get_tokens ((show x) ++ "%" ++ (show y)) == (num_to_tokens x) ++ [Mod] ++ (num_to_tokens y)

prop_Mul :: Int -> Int -> Bool
prop_Mul x y = get_tokens ((show x) ++ "*" ++ (show y)) == (num_to_tokens x) ++ [Mul] ++ (num_to_tokens y)

prop_Neq :: Int -> Int -> Bool
prop_Neq x y = get_tokens ((show x) ++ "!=" ++ (show y)) == (num_to_tokens x) ++ [Neq] ++ (num_to_tokens y)

prop_Sub :: Int -> Int -> Bool
prop_Sub x y = get_tokens ((show x) ++ "-" ++ (show y)) == (num_to_tokens x) ++ [Sub] ++ (num_to_tokens y)

return []
runTests = $quickCheckAll

main = runTests
