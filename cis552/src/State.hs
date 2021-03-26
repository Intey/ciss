{-# LANGUAGE InstanceSigs  #-}
{-# LANGUAGE TupleSections #-}

module State where

import           Control.Monad (ap, liftM)

{-| State monad -}
newtype State s a = S {runState :: s -> (a, s)} -- ^ default state constructor

instance Monad (State s) where
  return :: a -> State s a
  return x = S (x,)
  (>>=) :: State s a -> (a -> State s b) -> State s b
  st >>= f = S $ \s -> let (a,s') = runState st s in runState (f a) s'

instance Functor (State s) where
  fmap = liftM

instance Applicative (State s) where
  pure = return
  (<*>) = ap

evalState :: State s a -> s -> a
evalState s ss = fst $ runState s ss

execState :: State b a -> b -> b
execState s ss = snd $ runState s ss

get :: State s s
get = S $ \s -> (s, s)

put :: s -> State s ()
put s = S $ const ((), s)

modify :: (s -> s) -> State s ()
modify f = S $ \s -> ((), f s)
