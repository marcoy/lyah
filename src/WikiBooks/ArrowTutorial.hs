{-# LANGUAGE Arrows #-}
module WikiBooks.ArrowTutorial where

import Control.Arrow
import Control.Monad
import qualified Control.Category as Cat
import Data.List
import Data.Maybe
import System.Random

newtype Circuit a b = Circuit { unCircuit :: a -> (Circuit a b, b) }

instance Cat.Category Circuit where
        id = Circuit $ \a -> (Cat.id, a)
        (.) = dot
            where
                (Circuit cir2) `dot` (Circuit cir1) = Circuit $ \a ->
                    let (cir1', b) = cir1 a
                        (cir2', c) = cir2 b
                    in  (cir2' `dot` cir1', c)

instance Arrow Circuit where
        arr f = Circuit $ \a -> (arr f, f a)
        first (Circuit cir) = Circuit $ \(b, d) ->
            let (cir', c) = cir b
            in  (first cir', (c, d))

runCircuit :: Circuit a b -> [a] -> [b]
runCircuit _ [] = []
runCircuit cir (x:xs) =
        let (cir', x') = unCircuit cir x
        in  x' : runCircuit cir' xs

-- | Accumulator that outputs a value determined by the supplied function.
accum :: acc -> (a -> acc -> (b, acc)) -> Circuit a b
accum acc f = Circuit $ \input ->
    let (output, acc') = input `f` acc
    in  (accum acc' f, output)

-- | Accumulator that outputs the accumulator value.
accum' :: b -> (a -> b -> b) -> Circuit a b
accum' acc f = accum acc (\a b -> let b' = a `f` b
                                  in  (b', b'))

total :: Num a => Circuit a a
total = accum' 0 (+)
