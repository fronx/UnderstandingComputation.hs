{-# LANGUAGE GADTs, StandaloneDeriving #-}
module Simple where

data Expr a where
  Number   :: Int  -> Expr Int
  Boolean  :: Bool -> Expr Bool
  Add      :: Expr Int -> Expr Int -> Expr Int
  Multiply :: Expr Int -> Expr Int -> Expr Int
  LessThan :: Expr Int -> Expr Int -> Expr Bool

deriving instance Show a => Show (Expr a)

native :: Expr a -> a
native (Number   x  ) = x
native (Add      x y) = native(x) + native(y)
native (Multiply x y) = native(x) * native(y)
native (Boolean  x  ) = x
native (LessThan x y) = native(x) < native(y)

class Eval a where
  box :: a -> Expr a
  eval :: Expr a -> Expr a
  eval = box . native

instance Eval Int  where box = Number
instance Eval Bool where box = Boolean
