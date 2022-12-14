module Parser (Expr (..), Instr (..), tokensToInstr) where

import qualified Lexer as T (Token (..))

operator :: T.Token -> Bool
operator T.Add = True
operator T.Sub = True
operator T.Mul = True
operator T.Div = True
operator T.Mod = True
operator T.Neq = True
operator T.Lth = True
operator _ = False

data Priority = HIGH | LOW

priority :: T.Token -> Priority
priority T.Add = LOW
priority T.Sub = LOW
priority _ = HIGH

value :: T.Token -> Bool
value token = case token of
  T.Val x -> True
  T.Var x -> True
  _ -> False

-- | Expressions in SimpleLp. Can be evaluated to return a Maybe Int
data Expr
  = Val Int
  | Var String
  | Operator T.Token Expr Expr
  deriving (Show)

mkOperation :: T.Token -> Expr -> Expr -> Expr
mkOperation token exprL exprR
  | operator token = Operator token exprL exprR
  | otherwise = error "token must be an operator"

-- | Instructions in SimpleLP. Can be executed to return a LookUpTable
data Instr
  = Ass String Expr
  | Seq [Instr]
  | Cond Expr Instr Instr
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
tokensToInstr (T.Cond : tokens) = Cond expr instr1 instr2 : restOfInstr
  where
    (expr, restOfTokens) = extractExpression tokens
    (instr1 : instr2 : restOfInstr) = tokensToInstr restOfTokens
tokensToInstr (T.Loop : tokens) = Loop expr instr : restOfInstr
  where
    (expr, restOfTokens) = extractExpression tokens
    (instr : restOfInstr) = tokensToInstr restOfTokens
tokensToInstr (T.Print : tokens) = Print expr : restOfInstr
  where
    (expr, restOfTokens) = extractExpression tokens
    restOfInstr = tokensToInstr restOfTokens
tokensToInstr (t1 : _) = error $ "error on token: " ++ show t1

extractExpression :: [T.Token] -> (Expr, [T.Token])
-- an expression is value operator, value operator ... so when this breaks, we have finished
extractExpression tokens = (expression, rest)
  where
    (expressionTokens, rest) = separateExpressionTokens tokens
    expression = makeExpression expressionTokens

separateExpressionTokens :: [T.Token] -> ([T.Token], [T.Token])
separateExpressionTokens (T.LPt : rest) = unite [T.LPt] (separateExpressionTokens rest)
separateExpressionTokens (T.RPt : rest) = unite [T.RPt] (separateExpressionTokens rest)
separateExpressionTokens (valToken : T.RPt : opToken : rest) =
  if value valToken
    then
      if operator opToken
        then unite [valToken, T.RPt, opToken] (separateExpressionTokens rest)
        else unite [valToken, T.RPt] (separateExpressionTokens (opToken : rest))
    else error $ "you can't place a ) after " ++ show valToken
separateExpressionTokens (valToken : opToken : rest) =
  if operator opToken && value valToken
    then unite [valToken, opToken] (separateExpressionTokens rest)
    else ([valToken], opToken : rest)
separateExpressionTokens tokens = ([], tokens)

unite :: [T.Token] -> ([T.Token], [T.Token]) -> ([T.Token], [T.Token])
unite iniExpr (restExpr, restTokens) = (iniExpr ++ restExpr, restTokens)

makeExpression :: [T.Token] -> Expr
makeExpression [T.Val x] = Val x
makeExpression [T.Var x] = Var x
makeExpression (T.LPt : rest) = case separateParenthesis rest of
  (tokens, []) -> makeExpression $ init tokens
  (expressionTokens, rest) -> operate (makeExpression expressionTokens) rest
makeExpression (leftValToken : opToken : T.LPt : rest)
  | value leftValToken && operator opToken = case priority opToken of
    HIGH -> operate (Operator opToken (makeExpression [leftValToken]) (makeExpression beforeParenthesis)) afterParenthesis
    LOW -> Operator opToken (makeExpression [leftValToken]) (operate (makeExpression beforeParenthesis) afterParenthesis)
  | otherwise = error "some kind of error TODO: "
  where
    (beforeParenthesis, afterParenthesis) = separateParenthesis rest
makeExpression (leftValToken : opToken : rightValToken : rest) -- this makes no sense
  | value leftValToken && value rightValToken && operator opToken =
    case priority opToken of
      LOW -> Operator opToken (makeExpression [leftValToken]) (makeExpression (rightValToken : rest))
      HIGH -> operate (Operator opToken (makeExpression [leftValToken]) (makeExpression [rightValToken])) rest
  | otherwise = error $ "expected value operator value, but got" ++ show leftValToken ++ show opToken ++ show rightValToken
makeExpression [] = error "Expression expected, but not found"
makeExpression (token : rest) = error $ "You can't start an expression with: " ++ show token ++ "..." ++ show rest

operate :: Expr -> [T.Token] -> Expr
operate expr [] = expr
operate expr (opToken : rest)
  | operator opToken = Operator opToken expr (makeExpression rest)
  | otherwise = error $ "operator expected, but got: " ++ show opToken

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