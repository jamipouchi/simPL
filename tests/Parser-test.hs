{-# LANGUAGE TemplateHaskell #-}

import qualified Lexer as T (Token (..), getTokens)
import LookUpTable
import Parser
import SimpleLP (exec)
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, run)


same mem1 mem2 = monadicIO $ do
  res1 <- run mem1
  res2 <- run mem2
  assert $ res1 == res2

prop_Empty :: Bool
prop_Empty = null (tokensToInstr [])

prop_Ass :: Int -> Property
prop_Ass x =
  same
    (exec (Seq $ tokensToInstr (T.getTokens $ "var" ++ " := " ++ show x)) empty)
    (exec (Ass "var" (Val x)) empty)

prop_Cond :: Int -> Int -> Property
prop_Cond x y =
  same
    (exec (Seq $ tokensToInstr (T.getTokens $ "if " ++ show x ++ " < " ++ show y ++ " " ++ "var" ++ " := " ++ show x)) empty)
    (exec (Cond (Operator T.Lth (Val x) (Val y)) (Ass "var" (Val x))) empty)

{-
tokensToInstr [] = []
tokensToInstr (T.Loop : tokens) = Loop expr instr : restOfInstr
  where
    (expr, restOfTokens) = extractExpression tokens
    (instr : restOfInstr) = tokensToInstr restOfTokens
tokensToInstr (T.Print : tokens) = Print expr : restOfInstr
  where
    (expr, restOfTokens) = extractExpression tokens
    restOfInstr = tokensToInstr restOfTokens
tokensToInstr (t1 : _) = error $ "error on token: " ++ show t1
-}

return []

runTests = $quickCheckAll

main = runTests
