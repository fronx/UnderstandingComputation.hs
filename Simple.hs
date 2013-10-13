{-# LANGUAGE TypeFamilies #-}

module Simple where

data IntExpr = Number   Int
             | Add      IntExpr IntExpr
             | Multiply IntExpr IntExpr

data BoolExpr = Boolean  Bool
              | LessThan IntExpr IntExpr

class Expr a where
  type Native a
  native :: a -> Native a
  box    :: Native a -> a
  eval   :: a -> a
  eval = box . native

instance Expr IntExpr where
  type Native IntExpr = Int
  native (Number x)     = x
  native (Add      x y) = native(x) + native(y)
  native (Multiply x y) = native(x) * native(y)
  box x = Number x

instance Expr BoolExpr where
  type Native BoolExpr = Bool
  native (Boolean x)    = x
  native (LessThan a b) = native(a) < native(b)
  box x = Boolean x
