{-# LANGUAGE InstanceSigs #-}
module WikiBooks.Transformers where

import Control.Monad
import Control.Monad.Trans
import Data.Char

--getPassphrase :: IO (Maybe String)
--getPassphrase = do
--        s <- getLine
--        if isValid s
--            then return $ Just s
--            else return Nothing


--askPassphrase :: IO ()
--askPassphrase = undefined
-- do putStrLn "Insert your new passphrase:"
--                    maybeValue <- getPassphrase
--                    case maybeValue ...


newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Monad m) => Monad (MaybeT m) where
        -- point-free possible
        return x = MaybeT $ return $ Just x

        (>>=) :: MaybeT m a -> (a -> (MaybeT m b)) -> MaybeT m b
        x >>= k = MaybeT $ do
                maybeValue  <- runMaybeT x
                case maybeValue of
                    Nothing -> return Nothing
                    Just v  -> runMaybeT $ k v

instance Monad m => MonadPlus (MaybeT m) where
        mzero    = MaybeT $ return Nothing
        mplus x y= MaybeT $ do
                maybeValue <- runMaybeT x
                case maybeValue of
                    Nothing -> runMaybeT y
                    Just _  -> return maybeValue

instance MonadTrans MaybeT where
        lift = MaybeT . (liftM Just)


getValidPassphrase :: MaybeT IO String
getValidPassphrase = do
        s <- lift getLine
        guard (isValid s)
        return s

askPassphrase :: MaybeT IO ()
askPassphrase = do
        lift $ putStrLn "Insert your new passhrase:"
        value <- getValidPassphrase
        lift $ putStrLn $ "You entered: " ++ value

isValid :: String -> Bool
isValid s = length s >= 8 && any isAlpha s && any isNumber s && any isPunctuation s

mtMain :: IO ()
mtMain = do
        maybePassphrase <- runMaybeT askPassphrase
        putStrLn "Done"
