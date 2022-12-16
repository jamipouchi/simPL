module Lexer (Token (..), getTokens) where

-- | Tokens in SimpleLp.
data Token
  = Val Int
  | Var String
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

-- | The mapping from string -> Token. (This prob can be done better)
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

-- | Given a string, returns a list of Token.
lexer :: String -> String -> [Token]
lexer "" w = [] -- FIXME
lexer (' ' : cs) w = if w == "" then lexer cs w else error $ "Space found, but no token could be formed with " ++ show w
lexer (c : cs) w = case getToken (w ++ [c]) of
  (Just token, True) -> token : lexer cs ""
  (Nothing, True) -> lexer cs (w ++ [c])
  (Nothing, False) -> token : lexer remaining ""
    where
      (token, remaining) = if isDigit c && w == "" then extractInt (c : cs) else extractString (w ++ c : cs)
  (Just token, False) -> error "unreachable" -- getToken does not return this ever

getToken :: String -> (Maybe Token, Bool)
getToken w = if not (null valid) then (find w valid, True) else (Nothing, False)
  where
    valid = filter (\(str, token) -> isPrefixTo str w) tokens

extractInt :: String -> (Token, String)
extractInt w = (Val (read (takeWhile isDigit w) :: Int), dropWhile isDigit w)

extractString :: String -> (Token, String)
extractString = separate ""
  where
    separate :: String -> String -> (Token, String)
    separate var "" = (Var var, "")
    separate var (' ' : token) = (Var var, token)
    separate var (r : token) =
      if any (\(str, _) -> isPrefixTo (r : token) str) tokens
        then (Var (trim var), r : token)
        else separate (var ++ [r]) token
    trim :: String -> String
    trim = takeWhile (/= ' ') . dropWhile (== ' ')

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

isPrefixTo :: Eq a => [a] -> [a] -> Bool
isPrefixTo r l = take (length l) r == l

find :: String -> [(String, Token)] -> Maybe Token
find _ [] = Nothing
find w ((str, token) : tokens) = if w == str then Just token else find w tokens