{-# LANGUAGE GADTs, DataKinds, ScopedTypeVariables, KindSignatures, NoStarIsType #-}
module GADTs where

-- import Test.HUnit
import Data.Kind(Type)

data OExp =
     OInt Int             -- a number constant, like '2'
   | OBool Bool           -- a boolean constant, like 'true'
   | OAdd OExp OExp       -- add two expressions, like 'e1 + e2'
   | OIsZero OExp         -- test if an expression is zero
   | OIf OExp OExp OExp   -- if expression, 'if e1 then e2 else e3'
   deriving (Eq, Show)

oevaluate :: OExp -> Maybe (Either Int Bool)
oevaluate (OInt i)  = Just (Left i)
oevaluate (OBool b) = Just (Right b)
oevaluate (OAdd e1 e2) =
  case (oevaluate e1, oevaluate e2) of
    (Just (Left i1), Just (Left i2)) -> Just (Left (i1 + i2))
    _                                -> Nothing
oevaluate (OIsZero e1)   =
  case oevaluate e1 of
     Just (Left x) -> if x == 0 then Just (Right True) else Just (Right False)
     _             -> Nothing
oevaluate (OIf e1 e2 e3) =
   undefined
   
data SExp where
    SInt :: Int -> SExp
    SBool :: Bool -> SExp
    SAdd :: SExp -> SExp -> SExp
    SIsZero :: SExp -> SExp
    SIf :: SExp -> SExp -> SExp -> SExp -- ^ SIf cond true_exp false_exp
