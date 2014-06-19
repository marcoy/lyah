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
        _ <- runMaybeT askPassphrase
        putStrLn "Done"


-- My Reader Monad
runMyReader :: MyReader e a -> e -> a
runMyReader (MyReader f) env = f env

myreader :: (e -> a) -> MyReader e a
myreader f = MyReader f

askMyReaderM :: MyReader e e
askMyReaderM = myreader $ \e -> e

newtype MyReader e a = MyReader (e -> a)

instance Monad (MyReader e) where
        return x = myreader $ \_ -> x
        rd >>= k = myreader $ \env ->
                let x = runMyReader rd env
                 in runMyReader (k x) env


-- My Reader Transformer. This is just an excercise.
newtype MyReaderT e m a = MyReaderT { runMyReaderT :: (e -> m a) }

instance (Monad m) => Monad (MyReaderT e m) where
        return x = MyReaderT $ \_ -> return x
        rd >>= k = MyReaderT $ \env -> do
                a <- runMyReaderT rd env
                runMyReaderT (k a) env

instance MonadTrans (MyReaderT e) where
        lift :: Monad m => m a -> MyReaderT e m a
        lift m = MyReaderT $ \_ -> m
        -- or lift m = MyReaderT (const m)

askMyReader :: (Monad m) => MyReaderT e m e
askMyReader = MyReaderT $ \e -> return e

myEnvComputation :: MyReaderT [String] IO (Bool, String)
myEnvComputation = do
        lift $ putStrLn "Type something and see if it matches anything in the Environment"
        userInput <- lift getLine
        -- lift $ putStrLn $ "You entered " ++ userInput
        -- The >>=, defined above, takes care of unwrapping the inner
        -- monad. Hence, I can use env <- askMyReader to extract out the
        -- environment.
        env <- askMyReader
        -- lift $ putStrLn $ "The env " ++ (show env)
        return $ (userInput `elem` env, userInput)

testMyEnvComp :: IO ()
testMyEnvComp = do
        putStrLn "Running EnvComputation"
        (result, input) <- runMyReaderT myEnvComputation ["asdf", "zxcv"]
        if result
            then putStrLn "You have entered something in the environment"
            else putStrLn $ "Nope. '" ++ input ++ "' is not in there."
