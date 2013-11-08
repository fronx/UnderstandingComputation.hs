module FunctorComposition where

import Prelude hiding ( Functor
                      , fmap
                      )

data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving Show

class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor [] where
  fmap = map

instance Functor Maybe where
  fmap f ma = case ma of
    Just x  -> Just (f x)
    Nothing -> Nothing

instance Functor Tree where
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Branch ta tb) = Branch (fmap f ta) (fmap f tb)

data Compose f g a = Compose (f (g a)) deriving Show

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose x) = Compose (fmap (fmap f) x)

tree0 = Branch (Branch (Leaf 1)
                       (Branch (Leaf 2)
                               (Leaf 3)))
               (Leaf 4)

tree1 = Branch (Branch (Leaf [1,2,3])
                       (Branch (Leaf [4,5])
                               (Leaf [6,7])))
               (Leaf [8])

tree2 = Branch (Branch (Leaf [Just 1, Just 2, Nothing])
                       (Branch (Leaf [Just 4, Just 5])
                               (Leaf [Just 6, Nothing])))
               (Leaf [Just 8])


--composeN f 0 = \x -> x
--composeN f 1 = f
--composeN f n = f . (composeN f (n - 1))

--fmapComp 1 f x = unwrap $ fmap f $ Compose x
--fmapComp 2 f x = (composeN unwrap 2) $ fmap f (composeN Compose 2 $ x)
--fmapComp 3 f x = (composeN unwrap 3) $ fmap f (composeN Compose 3 $ x)

unwrap (Compose x) = x

main = do
  print $ fmap (* 2) $ tree0
  print $ fmap (* 2) $ Compose tree1
  print $ fmap (* 2) $ Compose tree1
  print $ unwrap $ unwrap $ fmap (* 2) $ (Compose . Compose) tree2
