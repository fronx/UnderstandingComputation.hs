{-# LANGUAGE GADTs, FlexibleInstances #-}
module Simple where

data Expr a where
  Number   :: Int  -> Expr Int
  Boolean  :: Bool -> Expr Bool
  Add      :: Expr Int -> Expr Int -> Expr Int
  Multiply :: Expr Int -> Expr Int -> Expr Int
  LessThan :: Expr Int -> Expr Int -> Expr Bool

instance Show (Expr Bool) where
  show (Boolean b) = "Expr[ Boolean " ++ show b ++ " ]"
instance Show (Expr Int) where
  show (Number i) = "Expr[ Number " ++ show i ++ " ]"

native :: Expr a -> a
native (Number   x  ) = x
native (Add      x y) = native(x) + native(y)
native (Multiply x y) = native(x) * native(y)
native (Boolean  x  ) = x
native (LessThan x y) = native(x) < native(y)

class Eval a where
  eval :: Expr a -> Expr a

instance Eval Int where
  eval x = Number $ native x

instance Eval Bool where
  eval x = Boolean $ native x
