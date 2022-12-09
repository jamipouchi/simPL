module LookUpTable (LookUpTable, empty, get, set) where

type Mem = String -> Maybe Int
type Keys = [String]
data LookUpTable = LUT {mem :: Mem, keys :: Keys}

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
            toString (LUT mem []) = "Ø"
            toString (LUT mem [k]) = showOne k
            toString (LUT mem (k:ks)) = showOne k ++ ", " ++ toString(LUT mem ks)

empty :: LookUpTable
empty = (LUT newMem [])
    where newMem _ = Nothing
-- newMem = \_ -> Nothing

get :: LookUpTable -> String -> Maybe Int
get (LUT mem keys) key = mem key

set :: LookUpTable -> String -> Int -> LookUpTable
set (LUT mem keys) key val = (LUT updatedMem updatedkeys)
    where
        updatedMem inp 
            |   inp == key = Just val
            |   otherwise = get (LUT mem keys) inp
        updatedkeys = if (elem key keys) then keys else (key:keys)
-- set mem key val = \inp -> if (inp == key) then (Just val) else (get mem inp)