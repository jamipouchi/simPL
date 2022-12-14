module SimpleLP (eval, exec) where

import Lexer (Token (Add, Div, Lth, Mod, Mul, Neq, Sub))
import LookUpTable
import Parser

-- | Given a LookUpTable evaluates an expression. It can return Nothing for unassigned lookups, division and mod operations.
--  This is context dependent! (depends on the LookUpTable used)
eval :: Expr -> LookUpTable -> Maybe Int
eval (Val x) _ = Just x
eval (Var key) lut = get lut key
eval (Operator Add expr1 expr2) lut = safeEval (+) expr1 (const True) expr2 (const True) lut
eval (Operator Sub expr1 expr2) lut = safeEval (-) expr1 (const True) expr2 (const True) lut
eval (Operator Mul expr1 expr2) lut = safeEval (*) expr1 (const True) expr2 (const True) lut
eval (Operator Div expr1 expr2) lut = safeEval div expr1 (const True) expr2 (/= 0) lut
eval (Operator Mod expr1 expr2) lut = safeEval mod expr1 (const True) expr2 (/= 0) lut
--
eval (Operator Neq expr1 expr2) lut = b2i $ eval' (/=) expr1 expr2 lut
eval (Operator Lth expr1 expr2) lut = b2i $ eval' (<) expr1 expr2 lut
eval _ _ = error "unreachable"

safeEval :: (Int -> Int -> Int) -> Expr -> (Int -> Bool) -> Expr -> (Int -> Bool) -> LookUpTable -> Maybe Int
safeEval op expr1 cond1 expr2 cond2 lut = do
  res1 <- eval expr1 lut
  res2 <- eval expr2 lut
  if cond1 res1 && cond2 res2 then return (op res1 res2) else Nothing

eval' :: (Int -> Int -> Bool) -> Expr -> Expr -> LookUpTable -> Maybe Bool
eval' op expr1 expr2 lut = do
  res1 <- eval expr1 lut
  res2 <- eval expr2 lut
  return (op res1 res2)

b2i :: Maybe Bool -> Maybe Int
b2i (Just False) = Just 0
b2i (Just True) = Just 1
b2i Nothing = Nothing

-- | Given a LookUpTable, executes an instruction. It returns a new LookUpTable.
--  It can return the same LookUpTable if an instruction equivalent to the NoOp (or the NoOp) is executed
exec :: Instr -> LookUpTable -> IO LookUpTable
exec (Ass key expr) lut = case eval expr lut of
  Just val -> do return $ set lut key val
  Nothing -> error $ "Can't set Nothing to: " ++ key
exec NoOp lut = do return lut
exec (Seq []) lut = exec NoOp lut
exec (Seq (instr : instrs)) lut = do
  updatedLut <- exec instr lut
  exec (Seq instrs) updatedLut
exec (Cond expr instr1 instr2) lut = exec correctInstr lut
  where
    correctInstr = case eval expr lut of
      Just val -> if val /= 0 then instr1 else instr2
      Nothing -> error $ "condition unevaluatable (i don't think that word exists)" ++ show expr
exec (Loop expr body) lut = exec (Cond expr body_and_repeat NoOp) lut
  where
    body_and_repeat = Seq [body, Loop expr body]
exec (Print expr) lut = do
  putStrLn $ case eval expr lut of
    Nothing -> "> nothing"
    Just x -> "> " ++ show x
  return lut
