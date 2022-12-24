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

data Priority = HIGH | LOW deriving (Enum, Show, Eq)

instance Ord Priority where
  compare left right = compare (fromEnum right) (fromEnum left)

priority :: T.Token -> Priority
priority token = case token of
  T.Add -> LOW
  T.Sub -> LOW
  _ -> HIGH

-- | Expressions in SimpleLp. Can be evaluated to return a Maybe Int
data Expr
  = Val Int
  | Var String
  | Operator T.Token Expr Expr
  | Read
  deriving (Show)

value :: T.Token -> Bool
value token = case token of
  T.Val _ -> True
  T.Var _ -> True
  T.Read -> True
  _ -> False

valueOrExpr :: Either T.Token Expr -> Bool
valueOrExpr (Right _) = True
valueOrExpr (Left val) = value val

-- | Instructions in SimpleLP. Can be executed to return a LookUpTable
data Instr
  = Ass String Expr
  | Seq [Instr]
  | Cond Expr Instr
  | Loop Expr Instr
  | Print (Either Expr String)
  | NoOp
  deriving (Show)

-- | Entry point of the parser. Given a list of tokens, returns a list of instructions
tokensToInstr :: [T.Token] -> [Instr]
tokensToInstr [] = []
tokensToInstr (T.Var var : T.Ass : tokens) = Ass var expr : restOfInstr
  where
    (expr, restOfTokens) = extractExpression tokens
    restOfInstr = tokensToInstr restOfTokens
tokensToInstr (T.LCu : tokens) = Seq (tokensToInstr insideParenthesis) : tokensToInstr afterParenthesis
  where
    (insideParenthesis, afterParenthesis) = separateParenthesis T.LCu tokens
tokensToInstr (T.Cond : tokens) = Cond expr instr : restOfInstr
  where
    (expr, restOfTokens) = extractExpression tokens
    (instr : restOfInstr) = tokensToInstr restOfTokens
tokensToInstr (T.Loop : tokens) = Loop expr instr : restOfInstr
  where
    (expr, restOfTokens) = extractExpression tokens
    (instr : restOfInstr) = tokensToInstr restOfTokens
tokensToInstr (T.For : T.Var var : T.Val minVal : T.Rge : T.Val maxVal : rest) =
  Seq
    [ Ass (var ++ "_save") (Var var), -- can't set var_save to nothing...
      Ass var (Val minVal),
      Loop (Operator T.Lth (Var var) (Val maxVal)) (Seq [firstInstr, Ass var (Operator T.Add (Var var) (Val 1))]),
      Ass var (Var (var ++ "_save"))
    ] :
  restOfInstr
  where
    ([firstInstr], restOfInstr) = splitAt 1 (tokensToInstr rest)
tokensToInstr (T.Print : T.LPt : tokens) = Print (Left expr) : tokensToInstr afterParenthesis
  where
    (insideParenthesis, afterParenthesis) = separateParenthesis T.LPt tokens
    expr = makeExpression insideParenthesis
tokensToInstr (T.Print : T.Str str : rest) = Print (Right str) : tokensToInstr rest
tokensToInstr (T.Print : _) = error "you need to place what you print inside parenthesis!"
tokensToInstr (t1 : rest) = error $ "error on token: " ++ show t1 ++ " " ++ show rest

extractExpression :: [T.Token] -> (Expr, [T.Token])
extractExpression tokens = (expression, rest)
  where
    (expressionTokens, rest) = separateExpressionTokens tokens
    expression = makeExpression expressionTokens

separateExpressionTokens :: [T.Token] -> ([T.Token], [T.Token])
separateExpressionTokens [T.RPt] = ([T.RPt], [])
separateExpressionTokens [valToken]
  | value valToken = ([valToken], [])
  | otherwise = error $ "expected value, but got: " ++ show valToken
separateExpressionTokens (T.Sub : T.LPt : rest) = unite [T.Sub] (separateExpressionTokens rest)
separateExpressionTokens (T.LPt : T.Sub : rest) =
  separateExpressionTokens ((T.LPt : T.Val 0 : [T.Sub] ++ insideParenthesis ++ [T.RPt]) ++ afterParenthesis)
  where
    (insideParenthesis, afterParenthesis) = separateParenthesis T.LPt rest
separateExpressionTokens (T.Sub : valToken : rest)
  | value valToken = separateExpressionTokens ([T.LPt, T.Val 0, T.Sub, valToken, T.RPt] ++ rest) -- fix this...
  | otherwise = error $ "expected value, but got: " ++ show valToken
separateExpressionTokens (T.LPt : rest) = unite [T.LPt] (separateExpressionTokens rest)
separateExpressionTokens (T.RPt : rest) = unite [T.RPt] (separateExpressionTokens rest)
separateExpressionTokens (valToken : T.RPt : opToken : rest) =
  if value valToken
    then
      if operator opToken
        then unite [valToken, T.RPt, opToken] (separateExpressionTokens rest)
        else ([valToken, T.RPt], opToken : rest)
    else error $ "you can't place a ')' after " ++ show valToken
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
makeExpression' (Left (T.Val x)) [] = Val x
makeExpression' (Left (T.Var x)) [] = Var x
makeExpression' (Left T.Read) [] = Read
makeExpression' (Left T.Sub) rest = makeExpression' (Right (Val 0)) (T.Sub : rest)
makeExpression' (Right expr) [] = expr
makeExpression' (Left T.LPt) rest = case separateParenthesis T.LPt rest of
  (tokens, []) -> makeExpression tokens
  (expressionTokens, restOfTokens) -> makeExpression' (Right (makeExpression expressionTokens)) restOfTokens
makeExpression' leftValToken [opToken, rightValToken]
  | valueOrExpr leftValToken && operator opToken && value rightValToken = Operator opToken (makeExpression' leftValToken []) (makeExpression [rightValToken])
  | otherwise = error $ "malformed operation, should be value operator value, but is: " ++ show leftValToken ++ "..." ++ show opToken ++ " " ++ show rightValToken
makeExpression' leftValToken (opToken : T.Sub : rest)
  | valueOrExpr leftValToken && operator opToken = makeExpression' leftValToken (opToken : T.LPt : T.Sub : nextExpr ++ [T.RPt] ++ restOfExpr)
  | otherwise = error "TODO : HERE"
  where
    (nextExpr, restOfExpr) = if value $ head rest then splitAt 1 rest else separateParenthesis (head rest) (tail rest)
makeExpression' leftValToken (leftOpToken : T.LPt : rest)
  | valueOrExpr leftValToken && operator leftOpToken =
    case maybeRightOpToken of
      Just rightOpToken ->
        if priority leftOpToken >= priority rightOpToken
          then makeExpression' (Right (Operator leftOpToken (makeExpression' leftValToken []) (makeExpression insideParenthesis))) afterParenthesis
          else Operator leftOpToken (makeExpression' leftValToken []) (makeExpression (T.LPt : rest))
      Nothing -> Operator leftOpToken (makeExpression' leftValToken []) (makeExpression insideParenthesis)
  | otherwise = error $ "some kind of error TODO: " ++ show leftOpToken ++ " ) " ++ show maybeRightOpToken ++ " " ++ show rest
  where
    (insideParenthesis, afterParenthesis) = separateParenthesis T.LPt rest
    maybeRightOpToken = if afterParenthesis /= [] then Just (head afterParenthesis) else Nothing
makeExpression' leftValToken (leftOpToken : rightValToken : rightOpToken : rest)
  | valueOrExpr leftValToken && value rightValToken && operator leftOpToken && operator rightOpToken =
    if priority leftOpToken >= priority rightOpToken
      then makeExpression' (Right (Operator leftOpToken (makeExpression' leftValToken []) (makeExpression [rightValToken]))) (rightOpToken : rest)
      else Operator leftOpToken (makeExpression' leftValToken []) (makeExpression (rightValToken : rightOpToken : rest))
  | otherwise = error $ "expected value operator value, but got: " ++ show leftValToken ++ "..." ++ show leftOpToken ++ " " ++ show rightValToken
makeExpression' _ rest = error $ "You can't start an expression with: " ++ "..." ++ show rest

separateParenthesis :: T.Token -> [T.Token] -> ([T.Token], [T.Token])
separateParenthesis parenthesis = (\(l, r) -> (init l, r)) . (splitAt =<< (length . takeWhile (/= 0) . myScan))
  where
    myScan :: [T.Token] -> [Int]
    myScan = scanl (flip op) 1
    op c
      | c == getOpen parenthesis = (+ 1)
      | c == getClose parenthesis = subtract 1
      | otherwise = id
    getOpen :: T.Token -> T.Token
    getOpen par = case par of
      T.LPt -> T.LPt
      T.RPt -> T.LPt
      T.LCu -> T.LCu
      T.RCu -> T.LCu
      nonParent -> error $ "expected a parenthesis, but got: " ++ show nonParent
    getClose par = case par of
      T.LPt -> T.RPt
      T.RPt -> T.RPt
      T.LCu -> T.RCu
      T.RCu -> T.RCu
      nonParent -> error $ "expected a parenthesis, but got: " ++ show nonParent
