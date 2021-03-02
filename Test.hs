import Tree

{- NOTE THESE TESTS ARE NOT EXHAUSTIVE -}
{-**************************************************************
   Some example good trees for testing 
****************************************************************-}
emptyTree = Empty
tree1 = Branch Empty 5 Empty
tree2 = Branch (Branch Empty 3 Empty) 4 (Branch Empty 5 Empty)
tree3 = Branch (Branch Empty 2 (Branch Empty 3 Empty)) 4 (Branch Empty 7 (Branch Empty 9 Empty))
tree4 = Branch 
           (Branch  
             (Branch
               (Branch
                 (Branch
                   (Branch
                     (Branch
                       (Branch Empty 1 Empty)
                       2 Empty)
                     3 Empty)
                   4 Empty)
                 5 Empty)
               6 Empty)
             7 Empty)
           8 Empty

{-***************************************************************
   Some example bad trees for testing 
****************************************************************-}
badtree1 = (Branch (Branch Empty 2 Empty) 8 (Branch Empty 1 Empty))
badtree2 = (Branch (Branch Empty 9 Empty) 2 (Branch Empty 8 Empty))

{- testIt takes as input a String and a test, prints the String and
   evaluates the test.
-}
testIt :: (Show t) => (String, t) -> IO ()
testIt (s,f) = do 
  putStr "\n" 
  putStr s
  putStr "\n"
  print (f)

{- main executes a sequence of tests. Each test is an ordered pair
   of (String, t), where t is the test. Add tests as you like or
   remove tests if you like.
-}
main = do 
  {- Tests of prettyFormat -}
  putStr "prettyFormat tree1\n"
  putStr (prettyFormat tree1)
  putStr "prettyFormat tree2\n"
  putStr (prettyFormat tree2)
  putStr "prettyFormat tree3\n"
  putStr (prettyFormat tree3)
  putStr "prettyFormat tree4\n"
  putStr (prettyFormat tree4)

  {- Tests of find and check -}
  mapM testIt [
    ("find 4 tree1", 
      find 4 tree1
    ),
    ("find 4 tree2", 
      find 4 tree2
    ),
    ("find 4 tree3",
      find 4 tree3
    ),
    ("check tree3",
      check tree3
    ),
    ("check badtree1",
      check badtree1
    )
    ]


  {- Tests of height -}
  mapM testIt [
    ("height tree4",
      height tree4
    ),
    ("height tree3",
      height tree3
    ),
    ("height tree1",
      height tree1
    )
    ]

  {- Tests of reduceTree -}
  mapM testIt [
    ("reduceTree 0 (\x y z -> x + y + z) tree1",
      reduceTree 0 (\x y z -> x + y + z) tree1
    ),
    ("reduceTree 1 (\x y z -> x * y * z) tree3",
      reduceTree 1 (\x y z -> x * y * z) tree3
    ),
    ("reduceTree 0 (\x y z -> x + y + z) tree4",
      reduceTree 0 (\x y z -> x + y + z) tree4
    )
    ]

  {- Tests of mapTree -}
  putStr "mapTree (\\x -> x * x) (insert 1 tree1)\n"
  putStr (prettyFormat (mapTree (\x -> x * x) tree1))
  putStr "mapTree (\\x -> x * x) (insert 1 tree3)\n"
  putStr (prettyFormat (mapTree (\x -> x * x) tree3))

  {- Tests of insert -}
  putStr "prettyFormat (insert 1 tree1)\n"
  putStr (prettyFormat (insert 1 tree1))
  putStr "prettyFormat (insert 2 (insert 1 tree1))\n"
  putStr (prettyFormat (insert 2 (insert 1 tree1)))
  putStr "prettyFormat (insert 8 (insert 2 (insert 1 tree1)))\n"
  putStr (prettyFormat (insert 8 (insert 2 (insert 1 tree1))))

  {- Tests of balance -}
  putStr "prettyFormat (balance tree1)\n"
  putStr (prettyFormat (balance tree1))
  putStr "prettyFormat (balance tree4)\n"
  putStr (prettyFormat (balance tree4))
  putStr "prettyFormat (balance tree3)\n"
  putStr (prettyFormat (insert 10 tree3))
  putStr (prettyFormat (balance (insert 10 tree3)))
  {- putStr "prettyFormat (balance (insert 1 tree1))\n"
  putStr (prettyFormat (balance (insert 1 tree1)))
  putStr "prettyFormat (balance (insert 2 (insert 1 tree1)))\n"
  putStr (prettyFormat (balance (insert 2 (insert 1 tree1)))) -}

