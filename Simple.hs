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
  eval   :: a -> a
  native :: a -> b
  box    :: b -> a

instance Expr IntExpr Int where
  native (Number x)     = x
  native (Add      x y) = native(x) + native(y)
  native (Multiply x y) = native(x) * native(y)
  eval  = box . native
  box x = Number x

instance Expr BoolExpr Bool where
  native (Boolean x)    = x
  native (LessThan a b) = native(a) < native(b)
  eval  = box . native
  box x = Boolean x
