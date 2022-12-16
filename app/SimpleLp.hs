module SimpleLP (eval, exec) where

import Lexer (Token (Add, Div, Lth, Mod, Mul, Neq, Sub))
import LookUpTable
import Parser

-- | Given a LookUpTable evaluates an expression. It can return Nothing for unassigned lookups, division and mod operations.
--  This is context dependent! (depends on the LookUpTable used)
eval :: Expr -> LookUpTable -> IO (Maybe Int)
eval (Val x) _ = return $ Just x
eval (Var key) lut = return $ get lut key
eval Read _ = do
  num <- getContents
  if all isDigit num
    then return $ Just (read num :: Int)
    else return Nothing
eval (Operator Add expr1 expr2) lut = safeEval (+) expr1 (const True) expr2 (const True) lut
eval (Operator Sub expr1 expr2) lut = safeEval (-) expr1 (const True) expr2 (const True) lut
eval (Operator Mul expr1 expr2) lut = safeEval (*) expr1 (const True) expr2 (const True) lut
eval (Operator Div expr1 expr2) lut = safeEval div expr1 (const True) expr2 (/= 0) lut
eval (Operator Mod expr1 expr2) lut = safeEval mod expr1 (const True) expr2 (/= 0) lut
--
eval (Operator Neq expr1 expr2) lut = b2i $ eval' (/=) expr1 expr2 lut
eval (Operator Lth expr1 expr2) lut = b2i $ eval' (<) expr1 expr2 lut
eval _ _ = error "unreachable"

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

safeEval :: (Int -> Int -> Int) -> Expr -> (Int -> Bool) -> Expr -> (Int -> Bool) -> LookUpTable -> IO (Maybe Int)
safeEval op expr1 cond1 expr2 cond2 lut = do
  res1 <- eval expr1 lut
  res2 <- eval expr2 lut
  return $ operate op res1 cond1 res2 cond2

operate :: (Int -> Int -> a) -> Maybe Int -> (Int -> Bool) -> Maybe Int -> (Int -> Bool) -> Maybe a
operate op expr1 cond1 expr2 cond2 = do
  res1 <- expr1
  res2 <- expr2
  if cond1 res1 && cond2 res2 then return (op res1 res2) else Nothing

eval' :: (Int -> Int -> Bool) -> Expr -> Expr -> LookUpTable -> IO (Maybe Bool)
eval' op expr1 expr2 lut = do
  res1 <- eval expr1 lut
  res2 <- eval expr2 lut
  return $ operate op res1 (const True) res2 (const True)

b2i :: IO (Maybe Bool) -> IO (Maybe Int)
b2i inp = do
  val <- inp
  case val of
    Just False -> return (Just 0)
    Just True -> return (Just 1)
    Nothing -> return Nothing

-- | Given a LookUpTable, executes an instruction. It returns a new LookUpTable.
--  It can return the same LookUpTable if an instruction equivalent to the NoOp (or the NoOp) is executed
exec :: Instr -> LookUpTable -> IO LookUpTable
exec (Ass key expr) lut = do
  res <- eval expr lut
  case res of
    Just val -> do return $ set lut key val
    Nothing -> error $ "Can't set " ++ key ++ " to nothing"
exec NoOp lut = do return lut
exec (Seq []) lut = exec NoOp lut
exec (Seq (instr : instrs)) lut = do
  updatedLut <- exec instr lut
  exec (Seq instrs) updatedLut
exec (Cond expr instr) lut = do
  res <- eval expr lut
  case res of
    Just val -> if val /= 0 then exec instr lut else exec NoOp lut
    Nothing -> error $ "condition unevaluatable (i don't think that word exists)" ++ show expr
exec (Loop expr body) lut = exec (Cond expr body_and_repeat) lut
  where
    body_and_repeat = Seq [body, Loop expr body]
exec (Print expr) lut = do
  res <- eval expr lut
  case res of
    Nothing -> putStrLn "> nothing"
    Just x -> putStrLn $ "> " ++ show x
  return lut
