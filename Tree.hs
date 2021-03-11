module Tree where

data BinarySearchTree a 
  = Empty
  | Branch (BinarySearchTree a) a (BinarySearchTree a)

-- Format tree into desired form
prettyFormat :: Show a => BinarySearchTree a -> [Char]
prettyFormat tree = prettyHelp tree ""

-- Helper for formatting
prettyHelp :: Show a => BinarySearchTree a -> [Char] -> [Char]
prettyHelp Empty indent = " "
prettyHelp (Branch left var right) indent = 
  (prettyHelp left (indent ++ "   ")) 
  ++ indent ++ 
  (show var)  ++ "\n" 
  ++ (prettyHelp right (indent ++ "    "))

-- Find function to located a specific value
find :: Ord a => a -> BinarySearchTree a -> Bool
find a Empty = False
find a (Branch left var right) = 
  if (var == a)
    then True
    else if (var < a)
      then find var right
      else find var left

-- Height function calculates the height of the tree object
height :: BinarySearchTree a -> Int
height tree = heightHelp tree 0

-- Helper function for height
heightHelp :: BinarySearchTree a -> Int -> Int
heightHelp Empty high = 0
heightHelp (Branch left var right) high = 
  1 + max 
  (heightHelp left (high)) 
  (heightHelp right (high))

-- Reduces tree to a value by operating on each node
reduceTree :: a -> (a -> a -> a -> a) -> BinarySearchTree a -> a
reduceTree value reduceHelp Empty = value
reduceTree value reduceHelp (Branch left var right) =
  (reduceHelp var
    (reduceTree value reduceHelp left)
    (reduceTree value reduceHelp right)
  )

-- Recieves a tree object and a function and moves through the tree constructing a new tree
mapTree :: (a -> b) -> BinarySearchTree a -> BinarySearchTree b
mapTree b Empty = Empty
mapTree b (Branch left var right) =
  Branch (mapTree b left)
  (b var)
  (mapTree b right)

-- Checks tree to see if it is in correct form or not
check :: (Ord a) => BinarySearchTree a -> Bool
check Empty = True
check (Branch Empty var Empty) = True

check (Branch Empty var right) = 
  if (var > node right)
    then False
  else check right

check (Branch left var Empty) =
  if (node left > var)
    then False
  else check left

check (Branch left var right) = 
  if (node right < var || node left > var)
    then False
  else (check right) && (check left)

-- Helper for check which will find a value off of any node for checking
node :: BinarySearchTree a -> a
node (Branch left var right) = var

-- leftTree for a left rotation
leftTree :: BinarySearchTree a -> BinarySearchTree a
leftTree (Branch (Branch treeA varOne treeB) varTwo treeC) =
  (Branch treeA varOne (Branch treeC varTwo treeB))

-- rightTree for a right rotation
rightTree :: BinarySearchTree a -> BinarySearchTree a
rightTree (Branch (Branch treeA varOne treeB) varTwo treeC) = 
  (Branch treeC varOne (Branch treeB varTwo treeA))

-- leftRightTree for a left->right rotation
leftRightTree :: BinarySearchTree a -> BinarySearchTree a
leftRightTree (Branch (Branch treeA varOne treeB) varTwo treeC) = 
  (Branch treeA varOne (Branch treeC varTwo treeB))

-- rightLeftTree for a right->left rotation
rightLeftTree :: BinarySearchTree a -> BinarySearchTree a
rightLeftTree (Branch (Branch treeA varOne treeB) varTwo treeC) = 
  (Branch treeB varOne (Branch treeA varTwo treeC))

-- Checks if tree is balanced and if not, balances the tree, I took my logic from an old 
-- c++ program so hopefully it works
balance :: BinarySearchTree a -> BinarySearchTree a
balance Empty = Empty
balance (Branch (Branch leftLeft leftVar leftRight) x (Branch rightLeft rightVar rightRight)) = 
  -- needs left rotation
  if (height (Branch leftLeft leftVar leftRight) > 1 + (height (Branch rightLeft rightVar rightRight)))
    then if (height leftLeft < height leftRight)
    
      then balance (leftTree (Branch (Branch leftLeft leftVar leftRight) 
      x (Branch rightLeft rightVar rightRight)))

    else balance (leftRightTree (Branch (Branch leftLeft leftVar leftRight) 
    x (Branch rightLeft rightVar rightRight)))

      else if ((height (Branch leftLeft leftVar leftRight) + 1) < (height (Branch rightLeft rightVar rightRight)))
        then if (height rightRight < height rightLeft)
     
          then balance (rightTree (Branch (Branch leftLeft leftVar leftRight) 
          x (Branch rightLeft rightVar rightRight)))
       
        else balance (rightLeftTree (Branch (Branch leftLeft leftVar leftRight) 
        x (Branch rightLeft rightVar rightRight)))
      else Branch (Branch leftLeft leftVar leftRight) 
      x (Branch rightLeft rightVar rightRight)