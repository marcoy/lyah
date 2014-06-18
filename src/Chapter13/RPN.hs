module Chapter13.RPN where

import Control.Monad
import Data.List (all)
import Debug.Trace
import Data.Ratio

traceS :: (Show a) => a -> a
traceS v = traceShow ("t> " ++ show v) v

solveRPN :: String -> Maybe Double
solveRPN st = do
        [result] <- traceS $ foldM foldingFunction [] (words st)
        return result

foldingFunction :: [Double] -> String -> Maybe [Double]
foldingFunction (x:y:ys) "*"    = return $ (x * y):ys
foldingFunction (x:y:ys) "+"    = return $ (x + y):ys
foldingFunction (x:y:ys) "-"    = return $ (y - x):ys
foldingFunction xs numberString = liftM (:xs) (readMaybe numberString)

readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of
                   [(x,"")] -> Just x
                   _        -> Nothing


newtype Prob a = Prob { getProb :: [(a,Rational)] }
                 deriving (Show)

instance Functor Prob where
        fmap f (Prob xs) = Prob $ map (\(x,p) -> (f x,p)) xs

instance Monad Prob where
        return x = Prob [(x,1%1)]
        m >>= k = flatten (fmap k m)
        fail _ = Prob []

flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concat $ map multAll xs
        where multAll (Prob innerxs,p) = map (\(x,r) -> (x,p*r)) innerxs

data Coin = Heads | Tails deriving (Show, Eq)

coin :: Prob Coin
coin = Prob [(Heads,1%2),(Tails,1%2)]

loadedCoin :: Prob Coin
loadedCoin = Prob [(Heads,1%10),(Tails,9%10)]

flipThree :: Prob Bool
flipThree = do
        a <- coin
        b <- coin
        c <- loadedCoin
        return (all (==Tails) [a,b,c])
