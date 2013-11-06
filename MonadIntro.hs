module MonadIntro where

import Prelude hiding ( Monad
                      , (>>=), unit
                      , Maybe, Just, Nothing
                      )

import FunctorIntro

class Monad m where
  unit :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

instance Monad [] where
  unit x = [x]
  xs >>= f = concat $ map f xs

doubleM :: (Monad m, Num a) => a -> m a
doubleM = unit . double

rfmap fa f = fmap f fa

main = do
  print $ [1,2,3] >>= doubleM >>= doubleM >>= (unit . (* 3))
  print $ [1,2,3] `rfmap` double `rfmap` double `rfmap` double
  print $ fmap double $ fmap double [1,2,3]
