module Chapter13.Monads where


import Control.Monad.State


type Stack = [Int]

pop :: State Stack Int
pop = state $ \(x:xs) -> (x,xs)

push :: Int -> State Stack ()
push a = state $ \xs -> ((),a:xs)

stackManip :: State Stack Int
stackManip = do
        push 5
        _ <- pop
        pop
