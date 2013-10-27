module Main where

import Simple

main :: IO ()
main = do
  print $ native (Add (Number 3)
                      (Number 4))
  print $ native (Multiply (Add (Number 1)
                                (Number 2))
                           (Add (Number 3)
                                (Multiply (Number 2)
                                          (Number 2))))
  print $ native (LessThan (Number 10)
                           (Add (Number 7)
                                (Number 2)))
  print $ eval (LessThan (Number 10)
                         (Add (Number 7)
                              (Number 2)))
