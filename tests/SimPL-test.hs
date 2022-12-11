{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import SimpleLP
import LookUpTable

prop_Val :: Int -> Bool
prop_Val x = (eval (Val x) empty) == Just x

prop_Var_assigned :: Int -> Bool
prop_Var_assigned x = (eval (Var "var") (set empty "var" x)) == Just x
prop_Var_unassigned :: Bool
prop_Var_unassigned = (eval (Var "unassigned") empty) == Nothing

prop_Add :: Int -> Int -> Bool
prop_Add x y = (eval (Add (Val x) (Val y)) empty) == Just (x + y)

prop_Sub :: Int -> Int -> Bool
prop_Sub x y = (eval (Sub (Val x) (Val y)) empty) == Just (x - y)

prop_Mul :: Int -> Int -> Bool
prop_Mul x y = (eval (Mul (Val x) (Val y)) empty) == Just (x * y)

prop_Div :: Int -> Int -> Bool
prop_Div x y = (eval (Div (Val x) (Val y)) empty) == if (y == 0) then Nothing else Just (div x y)

prop_Mod :: Int -> Int -> Bool
prop_Mod x y = (eval (Mod (Val x) (Val y)) empty) == if (y == 0) then Nothing else Just (mod x y)

--------------------------------------

prop_Lth :: Int -> Int -> Bool
prop_Lth x y = (eval (Lth (Val x) (Val y)) empty) == if (x < y) then (Just 1) else (Just 0)

prop_Neq :: Int -> Int -> Bool
prop_Neq x y = (eval (Neq (Val x) (Val y)) empty) == if (x /= y) then (Just 1) else (Just 0)

-------------------------------------------------------------------------------------------------

prop_Ass :: String -> Int -> Bool
prop_Ass key val = (exec (Ass key (Val val)) empty) == (set empty key val)
prop_Reass :: String -> Int -> Int -> Bool
prop_Reass key val1 val2 = (exec (Ass key (Val val2)) (set empty key val1)) == (set empty key val2)

prop_NoOp :: Bool
prop_NoOp = (exec (NoOp) empty) == empty

prop_Cond :: Int -> Bool
prop_Cond x = (exec (Cond (Val (mod x 2)) (Ass "var" (Val x)) (NoOp)) empty) == 
    if ((mod x 2) == 1) then (exec (Ass "var" (Val x)) empty) else (exec NoOp empty)

prop_Seq :: Int -> Int -> Bool
prop_Seq x y = (exec (Seq[(Ass "first" (Val x)), (Ass "second" (Val y)), (Ass "third" (Add (Var "first") (Var "second")))]) empty) == 
        (set (set (set empty "first" x) "second" y) "third" (x+y))

prop_Loop :: Int -> Bool
prop_Loop x = (exec (Seq[(Ass "i" (Val (abs x))), (Loop ((Lth (Val 0) (Var "i")) ) (Ass "i" (Sub (Var "i") (Val 1))))]) empty) == 
        (set empty "i" 0)

return []
runTests = $quickCheckAll

main = runTests
