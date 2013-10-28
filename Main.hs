module Main where

import Simple

main :: IO ()
main = do
  print $ eval (Add (Number 3)
                    (Number 4))
  print $ eval (Multiply (Add (Number 1)
                              (Number 2))
                         (Add (Number 3)
                              (Multiply (Number 2)
                                        (Number 2))))
  print $ eval (LessThan (Number 10)
                         (Add (Number 7)
                              (Number 2)))
  print $ eval (LessThan (Number 10)
                         (Add (Number 7)
                              (Number 2)))
