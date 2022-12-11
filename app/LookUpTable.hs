module LookUpTable (LookUpTable, empty, get, set) where

type Mem = String -> Maybe Int
type Keys = [String]
data LookUpTable = LUT Mem Keys

-- This is for testing
instance Eq LookUpTable where
    (LUT mem1 keysL) == (LUT mem2 keysR) = (sameKeys keysL keysR) && (sameVals keysL)
        where
            sameKeys l r = (length l == length r) && (and $ map (\k -> elem k r) l)
            sameVals keys = and $ map (\key -> (mem1 key == mem2 key)) keys

extract :: Maybe Int -> String
extract (Just x) = show x
extract Nothing = "?"

instance Show LookUpTable where
    show lut = "{" ++ toString lut ++ "}"
        where
            showOne :: String -> String
            showOne k = "'" ++ k ++ "': " ++ extract (get lut k)
            toString :: LookUpTable -> String
            toString (LUT _ []) = "Ø"
            toString (LUT _ [k]) = showOne k
            toString (LUT mem (k:ks)) = showOne k ++ ", " ++ toString(LUT mem ks)

-- |Returns a LookUpTable that maps from any key -> Nothing
empty :: LookUpTable
empty = (LUT newMem [])
    where
        newMem = \_ -> Nothing

-- |Given a LookUpTable and a key, returns Just val if it finds a mapping or else Nothing 
get :: LookUpTable -> String -> Maybe Int
get (LUT mem _) key = mem key

-- |Given a LookUpTable, a key and a val, creates a new LookUpTable with the additional mapping  
set :: LookUpTable -> String -> Int -> LookUpTable
set (LUT mem keys) key val = (LUT updatedMem updatedkeys)
    where
        updatedMem = \inp -> if (inp == key) then (Just val) else (mem inp)
        updatedkeys = if (elem key keys) then keys else (key:keys)