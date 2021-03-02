module Tree where

data BinarySearchTree a 
  = Empty
  | Branch (BinarySearchTree a) a (BinarySearchTree a)

{- Your function definitions go here, remember to comment each
   and add a function type specification to the start 
   see http://www.haskell.org/haddock/doc/html/markup.html 
-}