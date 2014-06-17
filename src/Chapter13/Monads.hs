module Chapter13.Monads where


import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import System.Random


--
-- Writer
--
isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Compared gang size to 9")

applyLog :: (a, String) -> (a -> (b,String)) -> (b,String)
applyLog (x,log) f = let (y,newLog) = f x in (y,log ++ newLog)


-- newtype Writer w a = Writer { runWriter :: (a, w) }

-- instance (Monoid w) => Monad (Writer w) where
--        return x = Writer (x, mempty)
--        (Writer (x,v)) >>= k = let (Writer (y, v')) = k x
--                                in Writer (y, v `mappend` v')

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
        a <- logNumber 3
        b <- logNumber 5
        tell ["Gonna multiply these two"]
        return (a * b)

gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
    | b == 0    = do
        tell ["Finished with " ++ show a]
        return a
    | otherwise = do
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        gcd' b (a `mod` b)

-- Difference list
newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Monoid (DiffList a) where
        mempty = DiffList (\xs -> [] ++ xs)
        (DiffList f) `mappend` (DiffList g) = DiffList $ \xs -> f (g xs)


--
-- State
--
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


--
-- Applicative using Monad
--
ap :: (Monad m) => m (a -> b) -> m a -> m b
ap mf m = do
        f <- mf
        x <- m
        return (f x)


keepSmall :: Int -> Writer [String] Bool
keepSmall x
    | x < 4 = do
        tell ["Keeping " ++ show x]
        return True
    | otherwise = do
        tell [show x ++ " is too large, throwing it away"]
        return False


powerset :: [a] -> [[a]]
powerset = filterM (\_ -> [True,False])
