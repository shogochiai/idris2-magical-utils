module Main

-- Pure computation, no IO
add : Integer -> Integer -> Integer
add x y = x + y

-- Entry point as IO that returns nothing (simpler)
main : IO ()
main = primIO $ \w => MkIORes () w
