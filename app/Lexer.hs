module Lexer (Token (..), getTokens) where

-- | Tokens in SimpleLp.
data Token
  = Val Int
  | Var String
  | Str String
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Lth
  | Neq
  | LPt
  | RPt
  | LCu
  | RCu
  | Quo
  | Ass
  | Cond
  | Loop
  | Print
  | Read
  deriving (Eq, Show)

-- | The mapping from string -> Token
tokens :: [([Char], Token)]
tokens =
  [ ("+", Add),
    ("-", Sub),
    ("*", Mul),
    ("/", Div),
    ("%", Mod),
    ("<", Lth),
    ("!=", Neq),
    ("(", LPt),
    (")", RPt),
    ("{", LCu),
    ("}", RCu),
    ("\"", Quo),
    (":=", Ass),
    ("if", Cond),
    ("while", Loop),
    ("print", Print),
    ("readNum", Read)
  ]

-- | Entry point to use the lexer
getTokens :: String -> [Token]
getTokens str = lexer str ""

-- | Given a string, and the current parsed word, returns a list of Token.
lexer :: String -> String -> [Token]
lexer "" _ = []
lexer (' ' : cs) w =
  if w == ""
    then lexer cs w
    else Var w : lexer cs ""
lexer ('"' : cs) w =
  if w == ""
    then string : lexer afterQuo ""
    else error $ "started string, but there was no token before, found: " ++ show w
  where
    (string, afterQuo) = extractString cs
lexer (c : cs) w =
  case getToken (w ++ [c]) of
    (Just token, True) -> token : lexer cs ""
    (Nothing, True) -> lexer cs (w ++ [c])
    (Nothing, False) -> token : lexer remaining ""
      where
        (token, remaining) = if isDigit c && w == "" then extractInt (c : cs) else extractVar (w ++ c : cs)
    (Just _, False) -> error "unreachable" -- getToken does not return this ever

getToken :: String -> (Maybe Token, Bool)
getToken w = if not (null valid) then (find w valid, True) else (Nothing, False)
  where
    valid = filter (\(str, _) -> isPrefixTo str w) tokens

extractInt :: String -> (Token, String)
extractInt w = (Val (read (takeWhile isDigit w) :: Int), dropWhile isDigit w)

extractVar :: String -> (Token, String)
extractVar = separate ""
  where
    separate :: String -> String -> (Token, String)
    separate var "" = (Var var, "")
    separate var (' ' : cs) = (Var var, cs)
    separate var (c : cs) = separate (var ++ [c]) cs

extractString :: String -> (Token, String)
extractString = separate ""
  where
    separate :: String -> String -> (Token, String)
    separate str ('"' : rest) = (Str str, rest)
    separate str (c : cs) = separate (str ++ [c]) cs
    separate _ [] = error "Expected '\"' but string was not closed!"

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

isPrefixTo :: Eq a => [a] -> [a] -> Bool
isPrefixTo r l = take (length l) r == l

find :: String -> [(String, Token)] -> Maybe Token
find _ [] = Nothing
find w ((str, token) : rest) = if w == str then Just token else find w rest