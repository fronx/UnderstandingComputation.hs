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
  eval :: a -> b

instance Expr IntExpr Int where
  eval (Number x)     = x
  eval (Add      x y) = eval(x) + eval(y)
  eval (Multiply x y) = eval(x) * eval(y)

instance Expr BoolExpr Bool where
  eval (Boolean x)    = x
  eval (LessThan a b) = eval(a) < eval(b)
