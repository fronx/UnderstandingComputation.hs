{-# LANGUAGE TypeFamilies #-}

module Simple where

data IntExpr = Number   Int
             | Add      IntExpr IntExpr
             | Multiply IntExpr IntExpr

data BoolExpr = Boolean  Bool
              | LessThan IntExpr IntExpr

type family Native a
type instance Native IntExpr  = Int
type instance Native BoolExpr = Bool

class Expr a where
  native :: a -> Native a
  box    :: Native a -> a
  eval   :: a -> a
  eval = box . native

instance Expr IntExpr where
  native (Number x)     = x
  native (Add      x y) = native(x) + native(y)
  native (Multiply x y) = native(x) * native(y)
  box x = Number x

instance Expr BoolExpr where
  native (Boolean x)    = x
  native (LessThan a b) = native(a) < native(b)
  box x = Boolean x
