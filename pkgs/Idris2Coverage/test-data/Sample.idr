module Sample

export
add : Int -> Int -> Int
add x y = x + y

export
multiply : Int -> Int -> Int
multiply x y = x * y

export
factorial : Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Unused function to test coverage
export
unused : Int -> Int
unused x = x + 100

main : IO ()
main = do
  putStrLn $ "add 2 3 = " ++ show (add 2 3)
  putStrLn $ "multiply 4 5 = " ++ show (multiply 4 5)
  putStrLn $ "factorial 5 = " ++ show (factorial 5)
