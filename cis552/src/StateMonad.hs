{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module StateMonad where

import           Control.Monad (ap, liftM)
import           Data.Functor  ((<&>))
import           Data.Map      (Map)
import qualified Data.Map      as Map
import qualified Data.Maybe    as Maybe
import           Debug.Trace   (trace)
import qualified State

-- import State()

data Tree a = Leaf a | Branch (Tree a) (Tree a)
  deriving (Show, Eq)

tree :: Tree Char
tree = Branch (Branch (Leaf 'a') (Leaf 'b')) (Leaf 'c')

countF :: Tree a -> Int
countF (Leaf _)     = 1
countF (Branch a b) = countF a + countF b

type Store = Int

countI :: Tree a -> Int
countI t = aux t 0
  where
    aux :: Tree a -> (Store -> Store)
    aux (Leaf _) = (+ 1)
    aux (Branch t1 t2) = \s ->
      let s' = aux t1 s
       in aux t2 s'

label1 :: Tree a -> Tree (a, Int)
label1 t = fst (aux t 0)
  where
    aux :: Tree a -> (Store -> (Tree (a, Int), Store))
    aux t@(Leaf c) s = (Leaf (c, s), s + 1)
    aux (Branch t1 t2) !s = (Branch t1' t2', s2)
      where
        (t1', s1) = aux t1 s
        (t2', s2) = aux t2 s1

type ST a = Store -> (a, Store)

returnST :: a -> ST a
returnST a s = (a, s)

bindST :: ST a -> (a -> ST b) -> ST b
bindST sf f s' = let (a, s'') = sf s' in f a s''

label2 :: Tree a -> Tree (a, Int)
label2 t = fst (aux t 0)
  where
    aux :: Tree a -> ST (Tree (a, Int))
    aux (Leaf x) = \s -> (Leaf (x, s), s + 1)
    aux (Branch t1 t2) = \s ->
      let (t1', s') = aux t1 s
       in let (t2', s'') = aux t2 s'
           in (Branch t1' t2', s'')

newtype ST2 a = S {apply :: Store -> (a, Store)}

instance Functor ST2 where
  fmap = liftM

instance Applicative ST2 where
  pure = return
  (<*>) = ap

instance Monad ST2 where
  return x = S (x,)
  f >>= g = S $ \s ->
    let (a, s') = apply f s
     in apply (g a) s'

fresh :: ST2 Int
fresh = S $ \s -> (s, s + 1)


mlabel :: Tree a -> ST2 (Tree (a, Int))
-- mlabel (Leaf x) = do
--     i <- fresh
--     return (Leaf (x, i))

-- mlabel (Branch t1 t2) = do
--   t1' <- mlabel t1
--   t2' <- mlabel t2
--   return (Branch t1' t2')

-- mlabel (Leaf x) = fresh >>= (\i -> return (Leaf (x, i)))
-- mlabel (Branch t1 t2) = mlabel t1 >>= (\t1' -> mlabel t2 >>= pure . Branch t1')

mlabel (Leaf x) = do
    i <- fresh
    return (Leaf (x, i))
-- import Data.Functor((<&>))
mlabel (Branch t1 t2) = mlabel t1 >>= (`fmap` mlabel t2) . Branch


freshS :: State.State Int Int
freshS = State.modify (+1) >> State.get


mlabelS :: Tree a -> State.State Int (Tree (a, Int))
mlabelS (Leaf x )= do y <- freshS
                      return $ Leaf (x, y)
mlabelS (Branch t1 t2) = do t1' <- mlabelS t1
                            t2' <- mlabelS t2
                            return (Branch t1' t2')


data MySt a = MS
  {
    index :: Int
  , freq  :: Map a Int
  } deriving (Show)

freshMS :: State.State (MySt a) Int
freshMS = do
  s <- State.get
  let idx' = index s + 1
  State.put $ s{index=idx'}
  return idx'

updFreqM :: Ord a => a -> State.State (MySt a) ()
updFreqM elem = do
  s <- State.get
  State.put s { freq = Map.alter (\case
                                    Just x -> Just (x+1)
                                    Nothing -> Just 1) 
                                  elem (freq s)}
  return ()

mlabelM :: Ord a => Tree a -> State.State (MySt a) (Tree (a, Int))
mlabelM (Leaf x)     =  do y <- freshMS
                           updFreqM x
                           return (Leaf (x,y))
mlabelM (Branch t1 t2) = do t1' <- mlabelM t1
                            t2' <- mlabelM t2
                            return (Branch t1' t2')


initM :: MySt a
initM = MS {index = 0, freq = Map.empty }

