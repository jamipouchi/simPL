{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use isNothing" #-}

import Interpreter
import Lexer (Token (Add, Div, Lth, Mod, Mul, Neq, Sub))
import LookUpTable
import Parser
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, pick, pre, run)

same mem1 mem2 = monadicIO $ do
  res1 <- run mem1
  res2 <- run mem2
  assert $ res1 == res2

sameVal expr res = monadicIO $ do
  res' <- run expr
  assert $ res == res'

prop_Val :: Int -> Property
prop_Val x = sameVal (eval (Val x) empty) (Just x)

prop_Var_assigned :: Int -> Property
prop_Var_assigned x = sameVal (eval (Var "var") (set empty "var" x)) (Just x)

prop_Var_unassigned :: Property
prop_Var_unassigned = sameVal (eval (Var "unassigned") empty) Nothing

prop_Add :: Int -> Int -> Property
prop_Add x y = sameVal (eval (Operator Add (Val x) (Val y)) empty) (Just (x + y))

prop_Sub :: Int -> Int -> Property
prop_Sub x y = sameVal (eval (Operator Sub (Val x) (Val y)) empty) (Just (x - y))

prop_Mul :: Int -> Int -> Property
prop_Mul x y = sameVal (eval (Operator Mul (Val x) (Val y)) empty) (Just (x * y))

prop_Div :: Int -> Int -> Property
prop_Div x y = sameVal (eval (Operator Div (Val x) (Val y)) empty) (if y == 0 then Nothing else Just (div x y))

prop_Mod :: Int -> Int -> Property
prop_Mod x y = sameVal (eval (Operator Mod (Val x) (Val y)) empty) (if y == 0 then Nothing else Just (mod x y))

--------------------------------------

prop_Lth :: Int -> Int -> Property
prop_Lth x y = sameVal (eval (Operator Lth (Val x) (Val y)) empty) (if x < y then Just 1 else Just 0)

prop_Neq :: Int -> Int -> Property
prop_Neq x y = sameVal (eval (Operator Neq (Val x) (Val y)) empty) (if x /= y then Just 1 else Just 0)

-------------------------------------------------------------------------------------------------

prop_Ass :: String -> Int -> Property
prop_Ass key val = same (exec (Ass key (Val val)) empty) (exec NoOp (set empty key val))

prop_Reass :: String -> Int -> Int -> Property
prop_Reass key val1 val2 = same (exec (Ass key (Val val2)) (set empty key val1)) (exec NoOp (set empty key val2))

prop_NoOp :: Property
prop_NoOp = same (exec NoOp empty) (exec NoOp empty)

prop_Cond :: Int -> Property
prop_Cond x =
  same
    (exec (Cond (Val (mod x 2)) (Ass "var" (Val x))) empty)
    (if mod x 2 == 1 then exec (Ass "var" (Val x)) empty else exec NoOp empty)

prop_Seq :: Int -> Int -> Property
prop_Seq x y =
  same
    (exec (Seq [Ass "first" (Val x), Ass "second" (Val y), Ass "third" (Operator Add (Var "first") (Var "second"))]) empty)
    (exec NoOp (set (set (set empty "first" x) "second" y) "third" (x + y)))

prop_Loop :: Int -> Property
prop_Loop x =
  same
    (exec (Seq [Ass "i" (Val (abs x)), Loop (Operator Lth (Val 0) (Var "i")) (Ass "i" (Operator Sub (Var "i") (Val 1)))]) empty)
    (exec NoOp (set empty "i" 0))

prop_Print_String :: String -> Property
prop_Print_String str =
  same
    (exec (Print (Right str)) empty)
    ( do
        putStrLn str
        return empty
    )

prop_Print_Expr :: Int -> Property
prop_Print_Expr x =
  same
    (exec (Print (Left (Val x))) empty)
    ( do
        print x
        return empty
    )

return []

runTests = $quickCheckAll

main :: IO Bool
main = runTests
