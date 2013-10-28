module Simple where

data Value = VInt Int | VBool Bool deriving Show

data Expr = Number  Int
          | Boolean Bool
          | Add      Expr Expr
          | Multiply Expr Expr
          | LessThan Expr Expr

eval :: Expr -> Value
eval (Number   x  ) = VInt  x
eval (Boolean  x  ) = VBool x
eval (Add      x y) = (eval x) `plusValue` (eval y)
eval (Multiply x y) = (eval x) `mulValue`  (eval y)
eval (LessThan x y) = (eval x) `lessThanValue` (eval y)

plusValue :: Value -> Value -> Value
plusValue (VInt a) (VInt b) = VInt (a + b)

mulValue :: Value -> Value -> Value
mulValue (VInt a) (VInt b) = VInt (a * b)

lessThanValue :: Value -> Value -> Value
lessThanValue (VInt a) (VInt b) = VBool (a < b)
lessThanValue x y = error "incompatible type"