module Parser (Expr (..), Instr (..), tokensToInstr) where

import qualified Lexer as T (Token (..), getTokens)

operator :: T.Token -> Bool
operator T.Add = True
operator T.Sub = True
operator T.Mul = True
operator T.Div = True
operator T.Mod = True
operator T.Neq = True
operator T.Lth = True
operator _ = False

data Priority = HIGH | LOW deriving (Enum, Show, Eq)

instance Ord Priority where
  compare left right = compare (fromEnum right) (fromEnum left)

priority :: T.Token -> Priority
priority T.Add = LOW
priority T.Sub = LOW
priority _ = HIGH

value :: T.Token -> Bool
value token = case token of
  T.Val _ -> True
  T.Var _ -> True
  _ -> False

valueOrExpr :: Either T.Token Expr -> Bool
valueOrExpr (Right _) = True
valueOrExpr (Left val) = value val

-- | Expressions in SimpleLp. Can be evaluated to return a Maybe Int
data Expr
  = Val Int
  | Var String
  | Operator T.Token Expr Expr
  deriving (Show)

-- | Instructions in SimpleLP. Can be executed to return a LookUpTable
data Instr
  = Ass String Expr
  | Seq [Instr]
  | Cond Expr Instr
  | Loop Expr Instr
  | Print Expr
  | NoOp
  deriving (Show)

tokensToInstr :: [T.Token] -> [Instr]
tokensToInstr [] = []
tokensToInstr (T.Var var : T.Ass : tokens) = Ass var expr : restOfInstr
  where
    (expr, restOfTokens) = extractExpression tokens
    restOfInstr = tokensToInstr restOfTokens
tokensToInstr (T.Cond : tokens) = Cond expr instr : restOfInstr
  where
    (expr, restOfTokens) = extractExpression tokens
    (instr : restOfInstr) = tokensToInstr restOfTokens
tokensToInstr (T.Loop : tokens) = Loop expr instr : restOfInstr
  where
    (expr, restOfTokens) = extractExpression tokens
    (instr : restOfInstr) = tokensToInstr restOfTokens
tokensToInstr (T.Print : tokens) = Print expr : restOfInstr
  where
    (expr, restOfTokens) = extractExpression tokens
    restOfInstr = tokensToInstr restOfTokens
tokensToInstr (t1 : rest) = error $ "error on token: " ++ show t1 ++ " " ++ show rest

extractExpression :: [T.Token] -> (Expr, [T.Token])
-- an expression is value operator, value operator ... so when this breaks, we have finished
extractExpression tokens = (expression, rest)
  where
    (expressionTokens, rest) = separateExpressionTokens tokens
    expression = makeExpression expressionTokens

-- separateExpressionTokens expr this fails with (-...)
-- ([Val 3,Mul,LPt,Val 4,Add,Val 2,RPt,Mul,Val 3,Add,LPt,Val 2,Add,Val 3,RPt,Mul],[Val 4])
separateExpressionTokens :: [T.Token] -> ([T.Token], [T.Token])
separateExpressionTokens [T.RPt] = ([T.RPt], [])
separateExpressionTokens [valToken]
  | value valToken = ([valToken], [])
  | otherwise = error $ "expected value, but got: " ++ show valToken
separateExpressionTokens (T.Sub : T.LPt : rest) = unite [T.Sub] (separateExpressionTokens rest)
separateExpressionTokens (T.Sub : valToken : rest)
  | value valToken = unite [T.LPt, T.Sub, valToken, T.RPt] (separateExpressionTokens ([T.Add, T.Val 0] ++ rest))
  | otherwise = error $ "expected value, but got: " ++ show valToken
separateExpressionTokens (T.LPt : rest) = unite [T.LPt] (separateExpressionTokens rest)
separateExpressionTokens (T.RPt : rest) = unite [T.RPt] (separateExpressionTokens rest)
separateExpressionTokens (valToken : T.RPt : opToken : rest) =
  if value valToken
    then
      if operator opToken
        then unite [valToken, T.RPt, opToken] (separateExpressionTokens rest)
        else unite [valToken, T.RPt] (separateExpressionTokens (opToken : rest))
    else error $ "you can't place a ) after " ++ show valToken
separateExpressionTokens (valToken : T.RPt : rest)
  | value valToken = ([valToken, T.RPt], rest)
  | otherwise = error $ "expected value, but got" ++ show valToken
separateExpressionTokens (valToken : opToken : rest)
  | operator opToken && value valToken = unite [valToken, opToken] (separateExpressionTokens rest)
  | value valToken = ([valToken], opToken : rest)
  | otherwise = ([], valToken : opToken : rest)
separateExpressionTokens tokens = ([], tokens)

unite :: [T.Token] -> ([T.Token], [T.Token]) -> ([T.Token], [T.Token])
unite iniExpr (restExpr, restTokens) = (iniExpr ++ restExpr, restTokens)

makeExpression :: [T.Token] -> Expr
makeExpression [] = error "Expected an expression but got nothing"
makeExpression (token : rest) = makeExpression' (Left token) rest

makeExpression' :: Either T.Token Expr -> [T.Token] -> Expr
makeExpression' leftValToken (operator : T.Sub : rest) = makeExpression' leftValToken (operator : T.LPt : T.Sub : nextExpr ++ [T.RPt] ++ restOfExpr)
  where
    (nextExpr, restOfExpr) = if value $ head rest then splitAt 1 rest else separateParenthesis $ tail rest
makeExpression' (Left (T.Val x)) [] = Val x
makeExpression' (Left (T.Var x)) [] = Var x
makeExpression' (Left T.Sub) rest = makeExpression' (Right (Val 0)) (T.Sub : rest)
makeExpression' (Right expr) [] = expr
makeExpression' (Left T.LPt) rest = case separateParenthesis rest of
  (tokens, []) -> makeExpression tokens
  (expressionTokens, restOfTokens) -> makeExpression' (Right (makeExpression expressionTokens)) restOfTokens
makeExpression' leftValToken [opToken, rightValToken]
  | valueOrExpr leftValToken && operator opToken && value rightValToken = Operator opToken (makeExpression' leftValToken []) (makeExpression [rightValToken])
  | otherwise = error $ "malformed operation, should be value operator value, but is: " ++ show leftValToken ++ "..." ++ show opToken ++ " " ++ show rightValToken
makeExpression' leftValToken (leftOpToken : T.LPt : rest)
  | valueOrExpr leftValToken && operator leftOpToken =
    case maybeRightOpToken of
      Just rightOpToken ->
        if priority leftOpToken >= priority rightOpToken
          then makeExpression' (Right (Operator leftOpToken (makeExpression' leftValToken []) (makeExpression insideParenthesis))) afterParenthesis
          else Operator leftOpToken (makeExpression' leftValToken []) (makeExpression (T.LPt : rest))
      Nothing -> Operator leftOpToken (makeExpression' leftValToken []) (makeExpression insideParenthesis)
  | otherwise = error "some kind of error TODO: "
  where
    (insideParenthesis, afterParenthesis) = separateParenthesis rest
    maybeRightOpToken = if afterParenthesis /= [] then Just (head afterParenthesis) else Nothing
makeExpression' leftValToken (leftOpToken : rightValToken : rightOpToken : rest)
  | valueOrExpr leftValToken && value rightValToken && operator leftOpToken && operator rightOpToken =
    if priority leftOpToken >= priority rightOpToken
      then makeExpression' (Right (Operator leftOpToken (makeExpression' leftValToken []) (makeExpression [rightValToken]))) (rightOpToken : rest)
      else Operator leftOpToken (makeExpression' leftValToken []) (makeExpression (rightValToken : rightOpToken : rest))
  | otherwise = error $ "expected value operator value, but got: " ++ show leftValToken ++ "..." ++ show leftOpToken ++ " " ++ show rightValToken
makeExpression' instr1 rest = error $ "You can't start an expression with: " ++ "..." ++ show rest

-- uuuh we need the handle -expr...

separateParenthesis :: [T.Token] -> ([T.Token], [T.Token])
separateParenthesis = separateCounting 1 0
  where
    separateCounting :: Int -> Int -> [T.Token] -> ([T.Token], [T.Token])
    separateCounting openPt closePt [token]
      | openPt - closePt == 1 && token == T.RPt = ([], [])
      | otherwise = error "Not enough closing parenthesis"
    separateCounting openPt closePt (token : rest)
      | openPt - closePt == 1 && token == T.RPt = ([], rest)
      | openPt > closePt = case token of
        T.LPt -> unite [token] (separateCounting (openPt + 1) closePt rest)
        T.RPt -> unite [token] (separateCounting openPt (closePt + 1) rest)
        _ -> unite [token] (separateCounting openPt closePt rest)
      | otherwise = error "Too much closing parenthesis"
    separateCounting _ _ [] = error "Couln't match parenthesis"

getTokens = T.getTokens