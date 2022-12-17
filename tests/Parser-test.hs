{-# LANGUAGE TemplateHaskell #-}

import Interpreter (exec)
import qualified Lexer as T (Token (..), getTokens)
import LookUpTable
import Parser
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, run)

same :: Instr -> Instr -> Property
same instr1 instr2 = monadicIO $ do
  res1 <- run calc1
  res2 <- run calc2
  assert $ res1 == res2
  where
    calc1 = exec instr1 empty
    calc2 = exec instr2 empty

prop_Empty :: Bool
prop_Empty = null (tokensToInstr [])

prop_Ass :: Int -> Property
prop_Ass x =
  same
    (Seq $ tokensToInstr $ T.getTokens $ "var" ++ " := " ++ show x)
    (Ass "var" (Val x))

prop_Cond :: Int -> Int -> Property
prop_Cond x y =
  same
    (Seq $ tokensToInstr $ T.getTokens $ "if " ++ show x ++ " < " ++ show y ++ " " ++ "var" ++ " := " ++ show x)
    (Cond (Operator T.Lth (Val x) (Val y)) (Ass "var" (Val x)))

prop_Loop :: Int -> Int -> Property
prop_Loop x y =
  same
    (Seq $ tokensToInstr $ T.getTokens $ "i := " ++ show (abs x) ++ " while 0 < i i := i - 1")
    (Seq [Ass "i" (Val (abs x)), Loop (Operator T.Lth (Val 0) (Var "i")) (Ass "i" (Operator T.Sub (Var "i") (Val 1)))])

prop_Print_String :: String -> Property
prop_Print_String str =
  same
    (Seq $ tokensToInstr $ T.getTokens $ "print \"" ++ str ++ "\"")
    (Print (Right str)) -- this works assuming str has no spaces.

prop_Print_Expr :: Int -> Property
prop_Print_Expr x =
  same
    (Seq $ tokensToInstr $ T.getTokens $ "print (" ++ show x ++ ")")
    (Print (Left (Val x)))

return []

runTests = $quickCheckAll

main = runTests
