module Chapter13.Monads where


import Control.Monad.State
import System.Random


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

stackStuff :: State Stack ()
stackStuff = do
        a <- pop
        if a == 5
            then push 5
            else do
                push 3
                push 8

moreStack :: State Stack ()
moreStack = do
        a <- stackManip
        if a == 100
            then stackStuff
            else return ()

stackyStack :: State Stack ()
stackyStack = do
        stackNow <- get
        if stackNow == [1,2,3]
            then put [8,3,1]
            else put [9,2,1]

mytest :: State Int Int
mytest = do
        put 3
        modify (+ 1)
        get

--
-- Random
--
randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

threeCoins :: State StdGen (Bool,Bool,Bool)
threeCoins = do
        a <- randomSt
        b <- randomSt
        c <- randomSt
        return (a,b,c)
