{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}

import Data.Tuple (swap)
import Control.Concurrent.STM
import Control.Concurrent.MVar
import Data.Foldable (forM_)
import Data.IORef

data Fire = Charmander | Charmeleon | Charizard deriving Show
data Water = Squirtle | Wartortle | Blastoise deriving Show
data Grass = Bulbasaur | Ivysaur | Venusaur deriving Show

-- data FireMove = Ember | FlameThrower | FireBlast deriving Show
-- data WaterMove = Bubble | WaterGun deriving Show
-- data GrassMove = VineWhip deriving Show

printBattle :: String -> String -> String -> String -> String -> IO ()
printBattle pokemonOne moveOne pokemonTwo moveTwo winner = do
        putStrLn $ pokemonOne ++ " used " ++ moveOne
        putStrLn $ pokemonTwo ++ " used " ++ moveTwo
        putStrLn $ "Winner is: " ++ winner ++ "\n"

class (Show pokemon, Show (Move pokemon)) => Pokemon pokemon where
        data Move pokemon :: *
        pickMove :: pokemon -> Move pokemon

instance Pokemon Fire where
        data Move Fire = Ember | FlameThrower | FireBlast deriving Show
        pickMove Charmander = Ember
        pickMove Charmeleon = FlameThrower
        pickMove Charizard  = FireBlast

instance Pokemon Water where
  data Move Water = Bubble | WaterGun deriving Show
  pickMove Squirtle = Bubble
  pickMove _ = WaterGun

instance Pokemon Grass where
  data Move Grass = VineWhip deriving Show
  pickMove _ = VineWhip

class (Show (Winner pokemon foe), Pokemon pokemon, Pokemon foe) => Battle pokemon foe where
  type Winner pokemon foe :: *
  type Winner pokemon foe = pokemon

  battle :: pokemon -> foe -> IO ()
  battle pokemon foe = do
    printBattle (show pokemon) (show move) (show foe) (show foeMove) (show winner)
   where
    foeMove = pickMove foe
    move = pickMove pokemon
    winner = pickWinner pokemon foe

  pickWinner :: pokemon -> foe -> (Winner pokemon foe)

instance Battle Water Fire where
        pickWinner pokemon foe = pokemon

instance Battle Fire Water where
        type Winner Fire Water = Water
        pickWinner = flip pickWinner

instance Battle Grass Water where
        pickWinner pokemon foe = pokemon

instance Battle Water Grass where
        type Winner Water Grass = Grass
        pickWinner = flip pickWinner

pokemonMain :: IO ()
pokemonMain = do
        battle Squirtle Charmander
        battle Bulbasaur Blastoise
-- main = pokemonMain

-- ============================================

class IOStore store where
        newIO :: a -> IO (store a)
        getIO :: store a -> IO a
        putIO :: store a -> a -> IO ()

instance IOStore MVar where
        newIO = newMVar
        getIO = readMVar
        putIO mvar a = modifyMVar_ mvar (return . const a)

instance IOStore IORef where
        newIO = newIORef
        getIO = readIORef
        putIO ioref a = modifyIORef ioref (const a)

type Present = String
storePresentsIO :: IOStore store => [Present] -> IO (store [Present])
storePresentsIO xs = do
        store <- newIO []
        forM_ xs $ \x -> do
            old <- getIO store
            putIO store (x : old)
        return store

class Store store where
        type StoreMonad store :: * -> *
        new :: a -> (StoreMonad store) (store a)
        get :: store a -> (StoreMonad store) a
        put :: store a -> a -> (StoreMonad store) ()

instance Store IORef where
        type StoreMonad IORef = IO
        new = newIORef
        get = readIORef
        put ioref a = modifyIORef ioref (const a)

instance Store TVar where
        type StoreMonad TVar = STM
        new = newTVar
        get = readTVar
        put tvar a = modifyTVar tvar (const a)

main :: IO ()
main = putStrLn "Type Families"
