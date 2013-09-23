{-# LANGUAGE
      MultiParamTypeClasses,
      FunctionalDependencies #-}

module Simple where

data IntExpr = Number   Int
             | Add      IntExpr IntExpr
             | Multiply IntExpr IntExpr

data BoolExpr = Boolean  Bool
              | LessThan IntExpr IntExpr

class Expr a b | a -> b where
  native :: a -> b
  box    :: b -> a
  eval   :: a -> a
  eval = box . native

instance Expr IntExpr Int where
  native (Number x)     = x
  native (Add      x y) = native(x) + native(y)
  native (Multiply x y) = native(x) * native(y)
  box x = Number x

instance Expr BoolExpr Bool where
  native (Boolean x)    = x
  native (LessThan a b) = native(a) < native(b)
  box x = Boolean x
