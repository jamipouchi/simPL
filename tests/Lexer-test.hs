{-# LANGUAGE TemplateHaskell #-}

import Lexer
import Test.QuickCheck

numToTokens :: Int -> [Token]
numToTokens x = if x >= 0 then [Val x] else [Sub, Val (abs x)]

prop_Val :: Int -> Bool
prop_Val x = getTokens (show x) == numToTokens x

prop_Var :: Bool -- Doesn't work cuz when testing strings like '%' '+' ... they fail.
prop_Var = getTokens "var" == [Var "var"]

prop_Add :: Int -> Int -> Bool
prop_Add x y = getTokens (show x ++ "+" ++ show y) == numToTokens x ++ [Add] ++ numToTokens y

prop_Ass :: Int -> Bool
prop_Ass x = getTokens ("var" ++ " := " ++ show x) == Var "var" : Ass : numToTokens x

prop_Cond :: Int -> Bool
prop_Cond x = getTokens ("if" ++ show x) == Cond : numToTokens x

prop_Div :: Int -> Int -> Bool
prop_Div x y = getTokens (show x ++ "/" ++ show y) == numToTokens x ++ [Div] ++ numToTokens y

prop_Loop :: Int -> Bool
prop_Loop x = getTokens ("while" ++ show x) == Loop : numToTokens x

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

prop_Print_String :: String -> Bool
prop_Print_String str = getTokens ("print \"" ++ str ++ "\"") == [Print, Str str]

prop_Print_Expr :: Int -> Bool
prop_Print_Expr x =
  getTokens ("print (" ++ show x ++ ")")
    == if x >= 0
      then [Print, LPt, Val x, RPt]
      else [Print, LPt, Sub, Val (abs x), RPt]

return []

runTests = $quickCheckAll

main :: IO Bool
main = runTests
