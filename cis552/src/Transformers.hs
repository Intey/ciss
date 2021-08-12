{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
-- {-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Transformers where
import           Control.Monad (ap, liftM)
import           State         (State)
import qualified State         as S

data Expr = Val Int
          | Div Expr Expr
          deriving (Show)

eval :: Expr -> Int
eval (Val i) = i
eval (Div e e1) = eval e `div` eval e1

class Monad m => MonadError e m where
    throwError :: e -> m a

class Monad m => MonadState s m where
    get :: m s
    put :: s -> m ()

tickStateInt :: MonadState Int m => m ()
tickStateInt = do
    (x :: Int) <- get
    put (x + 1)
    
errorS :: Show a => a -> a -> String
errorS y m = "Error dividing " ++ show y ++ " by " ++ show m

evalMega :: (MonadError String m, MonadState Int m) => Expr -> m Int
evalMega (Val n)   = return n
evalMega (Div x y) = do
  n <- evalMega x
  m <- evalMega y
  if m == 0
    then throwError $ errorS n m
    else do
      tickStateInt
      return (n `div` m)


newtype Mega a = Mega { runMega :: Int -> Either String (a, Int) }


instance Monad Mega where
  return :: a -> Mega a
  return x = Mega $ \i -> Right (x, i)
  
  (>>=) :: Mega a -> (a -> Mega b) -> Mega b
  ma >>= fmb = Mega $ \i -> case runMega ma i of
                                Right (a,i') -> runMega (fmb a) i'
                                Left e -> Left e



instance Applicative Mega where
  pure = return
  (<*>) = ap


instance Functor Mega where
  fmap = liftM


instance MonadError String Mega where
  throwError :: String -> Mega a
  throwError str = undefined


instance MonadState Int Mega where
  get   = undefined
  put x = undefined


goMega :: Expr -> IO ()
goMega e = putStr $ pr (evalMega e) where
   pr :: Mega Int -> String
   pr f = case runMega f 0 of
              Left s -> "Raise: " ++ s ++ "\n"
              Right (v, cnt) -> "Count: " ++ show cnt ++ "\n" ++
                                "Result: " ++ show v ++ "\n"

