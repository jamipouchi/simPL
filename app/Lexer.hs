module Lexer where

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
    | Ass
    | Cond
    | Loop
    deriving (Eq, Show)

-- | The mapping from string -> Token. (This prob can be done better)
tokens = [
    ("+", Add),
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
    (":=", Ass),
    ("if", Cond),
    ("while", Loop)
    ]

-- -- Space is 32, } is 125. So we accept chars >= 32 and <= 125
-- This is the function to use. Prob will be moved to main
file_to_string = do  
    input <- readFile "test.txt"
    let content = filter (\c -> ((fromEnum c) >= 32 && (fromEnum c) <= 125)) input
    print (lexer content "")

-- | Entry point to use the lexer
get_tokens :: String -> [Token]
get_tokens str = lexer str ""

-- | Given a string, returns a list of Token. It throws no errors, as he accepts anything for variable names. TODO: fix that
lexer :: String -> String -> [Token]
lexer "" w = [] -- FIXME
lexer (' ':cs) w = lexer cs w
lexer (c:cs) w = case (getToken (w ++ [c])) of
    (Just token, True) -> token : lexer cs ""
    (Nothing, True) -> lexer cs (w ++ [c])
    (Nothing, False) -> token : lexer remaining ""
        where 
            (token, remaining) = if ((isDigit c) && w == "") then extractInt (c:cs) else extractString (w ++ c:cs)

getToken :: String -> (Maybe Token, Bool)
getToken w = if ((length valid) /= 0) then ((find w valid), True)  else (Nothing, False)
    where
        valid = filter (\(str,token) -> isPrefixTo str w) tokens

extractInt :: String -> (Token, String)
extractInt w = (Val (read (takeWhile isDigit w) :: Int),dropWhile isDigit w)

extractString :: String -> (Token, String)
extractString w = separate "" w
    where
        separate :: String -> String -> (Token, String)
        separate var "" = (Var (trim var), "")
        separate (var) (r:token) = if (any (\(str,_) -> isPrefixTo (r:token) str) tokens) then
            (Var (trim var), r:token) else 
                separate (var ++ [r]) token
        trim :: String -> String
        trim = takeWhile (\c -> c /= ' ') . dropWhile (\c -> c == ' ')

isDigit :: Char -> Bool
isDigit c = (c >= '0' && c <= '9')

isPrefixTo :: Eq a => [a] -> [a] -> Bool
isPrefixTo r l = take (length l) r == l

find :: String -> [(String, Token)] -> Maybe Token
find _ [] = Nothing
find w ((str, token):tokens) = if (w == str) then (Just token) else (find w tokens)