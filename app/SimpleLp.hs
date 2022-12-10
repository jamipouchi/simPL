module SimpleLP (Expr(..), eval, Instr(..), exec) where
import LookUpTable

-- |Expressions in SimpleLp. Can be evaluated to return a Maybe Int
data Expr 
    = Val Int
    | Var String
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Mod Expr Expr
    | Neq Expr Expr
    | Lth Expr Expr
    deriving Show


-- |Two expressions are equal if they evaluate to the same value with an empty LookUpTable. This is context dependent! (depends on the LookUpTable used)
instance Eq Expr where
    expr1 == expr2 = (eval expr1 empty) == (eval expr2 empty)

-- |You compare expressions by comparing the values they evaluate to. This is context dependent! (depends on the LookUpTable used)
instance Ord Expr where
    compare expr1 expr2 = compare (eval expr1 empty) (eval expr2 empty)

-- |Given a LookUpTable evaluates an expression. It can return Nothing for unassigned lookups, division and mod operations.
-- This is context dependent! (depends on the LookUpTable used)
eval :: Expr -> LookUpTable -> Maybe Int
eval (Val x) _ = Just x
eval (Var key) lut = 
    case (get lut key) of
        Nothing -> Nothing
        Just x -> Just x
eval (Add expr1 expr2) lut = safeEval (+) expr1 (\_ -> True) expr2 (\_ -> True) lut
eval (Sub expr1 expr2) lut = safeEval (-) expr1 (\_ -> True) expr2 (\_ -> True) lut
eval (Mul expr1 expr2) lut = safeEval (*) expr1 (\_ -> True) expr2 (\_ -> True) lut
eval (Div expr1 expr2) lut = safeEval div expr1 (\_ -> True) expr2 (\x -> (x /= 0)) lut
eval (Mod expr1 expr2) lut = safeEval mod expr1 (\_ -> True) expr2 (\x -> (x /= 0)) lut
--
eval (Neq expr1 expr2) lut = b2i $ (eval' (/=) expr1 expr2 lut)
eval (Lth expr1 expr2) lut = b2i $ (eval' (<) expr1 expr2 lut)
--
-- a is cuz we still working with bools ...zzzz
eval' :: (Int -> Int -> Bool) -> Expr -> Expr -> LookUpTable -> Maybe Bool
eval' op expr1 expr2 lut = do
    res1 <- eval expr1 lut
    res2 <- eval expr2 lut
    return (op res1 res2)

safeEval :: (Int -> Int -> Int) -> Expr -> (Int -> Bool) -> Expr -> (Int -> Bool) -> LookUpTable -> Maybe Int
safeEval op expr1 cond1 expr2 cond2 lut = do
    res1 <- eval expr1 lut
    res2 <- eval expr2 lut
    if ((cond1 res1) && (cond2 res2)) then return (op res1 res2) else Nothing

b2i :: Maybe Bool -> Maybe Int
b2i (Just False) = Just 0
b2i (Just True) = Just 1
b2i Nothing = Nothing
-- I'll prob want to decouple booleans from ints later!

-- |Instructions in SimpleLP. Can be executed to return a LookUpTable 
data Instr
    = Ass String Expr
    | Seq [Instr]
    | Cond Expr Instr Instr
    | Loop Expr Instr
    | NoOp
    deriving Show

-- |Given a LookUpTable, executes an instruction. It returns a new LookUpTable.
-- It can return the same LookUpTable if an instruction equivalent to the NoOp (or itself) is executed
exec :: Instr -> LookUpTable -> LookUpTable
exec (Ass key expr) lut = case (eval expr lut) of
    Just val -> set lut key val
    Nothing -> error $ "Can't set Nothing to: " ++ key
exec NoOp lut = lut
exec (Seq []) lut = exec NoOp lut
exec (Seq (instr:instrs)) lut = exec (Seq instrs) (exec instr lut)
exec (Cond expr instr1 instr2) lut = exec correctInstr lut
    where 
        correctInstr = case (eval expr lut) of 
            Just val -> if val == 1 then instr1 else instr2
            Nothing -> error $ "condition unevaluatable (i don't think that word exists)" ++ show expr
exec (Loop expr body) lut = exec (Cond expr body_and_repeat NoOp) lut
    where
        body_and_repeat = (Seq[(body), (Loop expr body)])
