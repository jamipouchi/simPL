import SimpleLP
import LookUpTable

test_eval = do
    putStrLn "Testing eval:"

    putStrLn "  Evaluate a Val"
    putStrLn "  > eval (val 2) == Just 2: " 
    putStrLn $ show $ (eval (Val 2) empty ) == Just 2
    
    putStrLn "------------------------------------------"

    putStrLn "  Evaluate a Var"
    putStrLn "  > eval (Var \"should_be_2\") == Just 2: " 
    putStrLn $ show $ (eval (Var "should_be_2") (set empty "should_be_2" 2)) == Just 2
    putStrLn "  > eval (Var \"unassigned\") == Nothing: " 
    putStrLn $ show $ (eval (Var "unassigned") empty) == Nothing
    
    putStrLn "------------------------------------------"

    putStrLn "  Evaluate Add"
    putStrLn "  > eval (Add (Val 2) (Val 3)) == Just 5: " 
    putStrLn $ show $ (eval (Add (Val 2) (Val 3)) empty) == Just 5
    
    putStrLn "------------------------------------------"

    putStrLn "  Evaluate Sub"
    putStrLn "  > eval (Sub (Val 2) (Val 3)) == Just (-1): " 
    putStrLn $ show $ (eval (Sub (Val 2) (Val 3)) empty) == Just (-1)
    
    putStrLn "------------------------------------------"

    putStrLn "  Evaluate Mul"
    putStrLn "  > eval (Mul (Val 2) (Val 3)) == Just 6: " 
    putStrLn $ show $ (eval (Mul (Val 2) (Val 3)) empty) == Just 6
    
    putStrLn "------------------------------------------"

    putStrLn "  Evaluate Div"
    putStrLn "  > eval (Div (Val 2) (Val 3)) == Just 0: " 
    putStrLn $ show $ (eval (Div (Val 2) (Val 3)) empty) == Just 0
    putStrLn "  > eval (Div (Val 2) (Val 0)) == Just Exception: " 
    putStrLn $ show $ (eval (Div (Val 2) (Val 0)) empty)

test_instr = do
    putStrLn "Testing instr:"

    putStrLn "  Ass"
    putStrLn "  > exec (Ass \"should_be_2\" 2): "  
    putStrLn $ show $ (exec (Ass "should_be_2" (Val 2)) empty) == (set empty "should_be_2" 2)

    putStrLn "------------------------------------------"
    
    putStrLn "  NoOp"
    putStrLn "  > exec (NoOp): "
    putStrLn $ show $ (exec (NoOp) empty) == empty

    putStrLn "------------------------------------------"
    
    putStrLn "  Cond"
    putStrLn "  > exec (Cond (Val 1) (Ass \"is_2\" 2) (NoOp)) == exec (Ass \"is_2\" 2)"
    putStrLn $ show $ (exec (Cond (Val 1) (Ass "is_2" (Val 2)) (NoOp)) empty) == (exec (Ass "is_2" (Val 2)) empty)
    putStrLn "  > exec (Cond (Val 0) (Ass \"is_2\" 2) (NoOp)) == exec (NoOp)"
    putStrLn $ show $ (exec (Cond (Val 0) (Ass "is_2" (Val 2)) (NoOp)) empty) == (exec NoOp empty)

    putStrLn "------------------------------------------"

    putStrLn "  Seq"
    putStr "  (exec (Seq[(Ass \"one\" (Val 1)), (Ass \"two\" (Val 2)), (Ass \"should_be_three\" (Add (Var \"one\") (Var \"two\")))]) empty)"
    putStrLn " == (set (set (set empty \"one\" 1) \"two\" 2) \"should_be_three\" 3)"
    putStrLn $ show $ (exec (Seq[(Ass "one" (Val 1)), (Ass "two" (Val 2)), (Ass "should_be_three" (Add (Var "one") (Var "two")))]) empty) == 
            (set (set (set empty "one" 1) "two" 2) "should_be_three" 3)

    putStrLn "------------------------------------------"

    putStrLn "  Loop"
    putStrLn "  >exec (Loop (Val 0) (Ass \"is_2\" 2)) == exec (NoOp)"
    putStrLn $ show $ (exec (Loop (Val 0) (Ass "is_2" (Val 2))) empty) == (exec NoOp empty)
    putStrLn "  >exec (Loop (Var \"1_to_0\") (Ass \"1_to_0\" 0)) == ((Ass \"1_to_0\" 0))"
    putStrLn $ show $ (exec (Seq[(Ass "1_to_0" (Val 1)), (Loop (Var "1_to_0") (Ass "1_to_0" (Val 0)))]) empty) == (set empty "1_to_0" 0)

