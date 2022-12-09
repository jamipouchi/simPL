module SimpleLP (Expr(..), eval, Instr(..), exec) where
import LookUpTable

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

instance Eq Expr where
    expr1 == expr2 = (eval expr1 empty) == (eval expr2 empty)

instance Ord Expr where
    compare expr1 expr2 = compare (eval expr1 empty) (eval expr2 empty)

eval :: Expr -> LookUpTable -> Maybe Int
eval (Val x) lut = Just x
eval (Var key) lut = 
    case (get lut key) of
        Nothing -> Nothing
        Just x -> Just x
eval (Add expr1 expr2) lut = eval' (+) expr1 expr2 lut
eval (Sub expr1 expr2) lut = eval' (-) expr1 expr2 lut
eval (Mul expr1 expr2) lut = eval' (*) expr1 expr2 lut
eval (Div expr1 expr2) lut = eval' div expr1 expr2 lut
eval (Mod expr1 expr2) lut = eval' mod expr1 expr2 lut
--
eval (Neq expr1 expr2) lut = b2i $ (eval' (/=) expr1 expr2 lut)
eval (Lth expr1 expr2) lut = b2i $ (eval' (<) expr1 expr2 lut)
--
eval' :: (Int -> Int -> a) -> Expr -> Expr -> LookUpTable -> Maybe a
eval' op expr1 expr2 lut = do
    res1 <- eval expr1 lut
    res2 <- eval expr2 lut
    return (op res1 res2)

--
b2i :: Maybe Bool -> Maybe Int
b2i (Just False) = Just 0
b2i (Just True) = Just 1
b2i Nothing = Nothing
-- I'll prob want to decouple booleans from ints later!

data Instr
    = Ass String Expr
    | Seq [Instr]
    | Cond Expr Instr Instr
    | Loop Expr Instr
    | NoOp
    deriving Show

exec :: Instr -> LookUpTable -> LookUpTable
exec (Ass key expr) lut = case (eval expr lut) of
    Just val -> set lut key val
    Nothing -> error $ "Can't set Nothing to: " ++ key
exec (Seq []) lut = lut
exec (Seq (instr:instrs)) lut = exec (Seq instrs) (exec instr lut)
exec (Cond expr instr1 instr2) lut = exec correctInstr lut
    where 
        correctInstr = case (eval expr lut) of 
            Just val -> if val == 1 then instr1 else instr2
            Nothing -> error $ "condition unevaluatable (i don't think that word exists)" ++ show expr
exec (Loop expr body) lut = exec (Cond expr body_and_repeat NoOp) lut
    where
        body_and_repeat = (Seq[(body), (Loop expr body)])
exec NoOp lut = lut
