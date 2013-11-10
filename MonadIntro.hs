module MonadIntro where

import Prelude hiding ( Monad
                      , (>>=), unit
                      , Functor, fmap
                      , Maybe, Just, Nothing
                      )

import FunctorIntro

class Functor m => Monad m where
  unit :: a -> m a
  join :: m (m a) -> m a
  (>>=) :: m a -> (a -> m b) -> m b
  ma >>= f = join $ fmap f ma

instance Monad [] where
  unit x = [x]
  join = concat

instance Monad Maybe where
  unit = Just
  join (Just x) = x
  join Nothing = Nothing

doubleM :: (Monad m, Num a) => a -> m a
doubleM = unit . double

rfmap fa f = fmap f fa
rfmap2 fa f = (fmap . fmap) f fa

main = do
  -- bind
  putStrLn "-- bind"
  print $ [1,2,3] >>= doubleM >>= doubleM >>= (unit . (* 3))
  print $ Just 3  >>= doubleM >>= doubleM >>= (unit . (* 3))
  putStrLn "-- (nested bind)"
  print $ [[3], [2, 1]] >>=
             \mi -> mi >>= doubleM >>= (unit . (* 3))
  putStrLn "-- (join, then bind)"
  print $ join [[3], [2, 1]] >>= doubleM >>= (unit . (* 3))
  putStrLn "-- (nested do notation)"
  print $ do x <- [[3], [2, 1]]
             do y <- x
                z <- doubleM y
                z' <- (unit . (* 3)) z
                return z'
  --print $ [Just 3, Just 2, Nothing] >>=
  --           \mi -> mi >>= doubleM >>= (unit . (* 3))
  -- this doesn't work, because [] is expected inside, not Maybe

  -- fmap
  putStrLn "-- fmap"
  print $ [1,2,3] `rfmap` double `rfmap` double `rfmap` double
  print $ fmap double $ fmap double [1,2,3]
  print $ [Just 3, Just 2, Nothing] `rfmap2` double `rfmap2` double
