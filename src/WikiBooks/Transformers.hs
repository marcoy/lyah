{-# LANGUAGE InstanceSigs #-}
module WikiBooks.Transformers where

import Data.Char

getPassphrase :: IO (Maybe String)
getPassphrase = do
        s <- getLine
        if isValid s
            then return $ Just s
            else return Nothing

isValid :: String -> Bool
isValid s = length s >= 8 && any isAlpha s && any isNumber s && any isPunctuation s

askPassphrase :: IO ()
askPassphrase = undefined
-- do putStrLn "Insert your new passphrase:"
--                    maybeValue <- getPassphrase
--                    case maybeValue ...


newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Monad m) => Monad (MaybeT m) where
        -- point-free possible
        return x = MaybeT $ return $ Just x
        (>>=) :: MaybeT m a -> (a -> (MaybeT m b)) -> MaybeT m b
        (>>=) = undefined
        x >>= k = MaybeT $ do



mtMain :: IO ()
mtMain = putStrLn "Monad transformers"
