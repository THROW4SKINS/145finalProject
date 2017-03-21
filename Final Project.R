#bintree will be a function that creates the binary tree class. We will have it start as c(NA,NA,NA)
bintree <- function() {
  bt <- list()
  class(bt) <- "bintree"
  bt$depth <- 1
  bt$tree <- matrix(c(NA,NA,NA), nrow = 1, ncol = 3)    
  return(bt)
}
#generic ctor
#returns an empty tree

#newbintree will be a function that takes a matrix and create a new bintree based on that matrix
newbintree <- function(inmat) {
  #internal use ctor to get around S3 immutability
  #input:  a bintree in matrix form (bt$tree)
  #output: a new bintree
  bt <- list()
  rows = nrow(inmat)
  class(bt) <- "bintree"
  bt$depth <- rows
  bt$tree <- matrix(inmat, nrow = rows, ncol = 3)
  return(bt)
}

#push.bintree will be the binary tree's push function. It will call upon treeSort as the helper function to help sort our binary tree
#push.bintree will check if the current tree is NA, or empty. If it is, it will simply add in the element.
#We denote -1 as a form of "NA". We use -1 because using a lower integer will support our pop.bintree function.
#Note: By using -1 as a form of "NA", we deemed 0 as the lowest possible value in the binary tree. This means we assume this tree will only contain positive integers.
push.bintree <- function(curTree, elem) {
  #Input: curTree as binary tree
  #       elem as the element to insert into the binary tree
  #Output: Returns new binary tree with added element
  #Function: Take a binary tree and insert an element
  
  if(is.na(curTree$tree[1])) {
    curTree$tree[1,] <- c(elem,-1,-1)
    return(curTree)
    #If the tree is NA, which means empty, this will be the first insert into an empty tree.
  }
    #If the tree is not empty, we will call our helper function treeSort that adds the element and sorts it.
  else{
    return(treeSort(curTree, elem, 1, curTree$depth + 1))
  }
}

#treesort will be our primary algorithm to add in a new element and sort the binary tree
#We implement the concept of a binary tree here. In the push.bintree function, we have already created the root(first insert)
#Now, we will compare the element we are trying to add in (from here on our we call it elem) with the root number.
#As any binary tree works, if elem is lower than root, we move to the left child, and if higher, we move to the right child.
#If the left child or right child is -1, then that means it is empty, and we can insert into there.
#If it is not empty, we will recursively call treeSort, using the left child or right child, depending on which way we are moving, as the new "root" for the recursion
#Eventually, we will loop through the binary tree matrix just like a binary tree. Then we will create a new binary tree that adds in this element.
treeSort <- function(curTree, elem, curRow, nxtCol){
  #Input: curTree as binary tree.
  #       elem as the element to insert into the binary tree
  #       curRow, a variable, indicating the current row index
  #       nxtCol, a variable, indicating the row index to add the new element
  #Output: Returns new binary tree with added element
  #Function: Take a binary tree and insert an element in sorted form
  
  #Check if element is lower than root.
  if(elem <= curTree$tree[curRow,1]){
    #check if the left child is empty. The index [,2] indicates the left child.
    if(curTree$tree[curRow,2] == -1){
      #We will call newbintree with our newly added element by row binding the current matrix with a new row.
      newTree <- newbintree(rbind(curTree$tree, c(elem, -1, -1)))
      #Update the current Row's node, stating it now has a left child.
      newTree$tree[curRow,2] <- nxtCol
      return(newTree)
    }
    else{
      #Move to the left child and denote that as the "root" for the next treeSort.
      treeSort(curTree, elem, curTree$tree[curRow,2], nxtCol)
    }
  }
  #If element is higher than root
  else{
    #check if the rightt child is empty. The index [,3] indicates the right child.
    if(curTree$tree[curRow,3] == -1){
      newTree <- newbintree(rbind(curTree$tree, c(elem, -1, -1)))
      #Update the current Row's node, stating it now has a right child.
      newTree$tree[curRow,3] <- nxtCol
      return(newTree)
    }
    else{
      #Move to the right child and denote that as the "root" for the next treeSort.
      treeSort(curTree, elem, curTree$tree[curRow,3], nxtCol)
    }
  }
}

#pop.bintree will remove the smallest item in the btree. This function will call on helper function fixtree.
#The idea with pop is we are faced with two situations when we pop the smallest element from the binary tree
#Either the smallest element will have a right child or it will have no child.
#If the smallest element have no child, great, create new binary tree without this element
#However, if the smallest element does have a child, and in this case it must be a right child since the left child will make this element not the smallest,
#Then we will have to allocate the right child to the parent of the smallest element as a left child.
#Hence, in pop, we will start by moving all the way to the left of the tree since the left child is always the smaller element.
#While we are moving, we will also save the parent index.
#When we reach the most left node, indicating the smallest element, we will test if there is a right child, and allocate it to the parent if there is.
#Lastly, any left child and right child index creater than the index of the smallest element must be subtracted by one because the matrix decreases by one.
#And all indexes that gets moved up(or down an index) must be adjusted. The adjustment of the index will make use of R's column major structure.

pop.bintree <- function(btree) {
  #Input: Binary Tree
  #Output: Binary Tree without the smallest element
  #Function: Remove the smallest element in the binary tree. Fix the tree's sorting (indexes)
  
  #Variables to indicate the current row index and parent index
  curRow <- 1
  parRow <- 0
  
  #Iterate all the way down to the left of the binary tree. While loop stops when we hit a -1
  while(btree$tree[curRow,2] != -1){
    parRow <- curRow
    curRow <- btree$tree[curRow,2]
  }
  
  #Check if there is a right child. The index [,3] holds the right child.
  #If not, create a new binary tree without the popped element.
  if(btree$tree[curRow,3] == -1){
    newTree <- newbintree(btree$tree[-curRow,])
    #Call fixtree to fix the indexes after the element is popped
    newTree <- fixtree(newTree, curRow)
  }
  #If there is a right child
  else{
    #Allocate the parent's left child as the smallest index's right child.
    btree$tree[parRow,2] <- btree$tree[curRow,3]
    newTree <- newbintree(btree$tree[-curRow,])
    #Call fixtree to fix the indexes after the element is popped, indicating we we adjusted there was a child.
    newTree <- fixtree(newTree, curRow, child=TRUE)
  }
  return(newTree)
}

#Fixtree is pop.bintree's helper function. It will take the newly created btree, iterate the Right Child and Left Child indexes.
#If any of these indexes are greater than the delete element's index, then these indexes are moved up in the matrix, and hence it must be subtracted by one
#If the smallest element did not have a child, indicated by child = FALSE, then the parent that lost this child will replace that child's index with -1.
#If the smallest element did have a child, then the pop function already corrected the index of the parent of the popped child, so move on here.
#Fixtree is begin it's iteration at btree$depth + 1, which indicates the nrow of betree$tree +1. This is to utilize R's column major structure.
#Which allows us to iterrate through the matrix's [,2] and [,3] row, which holds the left and right child.

fixtree <- function(newbtree, delRow, child = FALSE){
  #Input: newbtree, a new binary tree with the removed element
  #       delRow, the row index of the removed element
  #       child, boolean, indicates if the removed element has a child or not
  #Output: Returns the new tree with adjusted indexes
  #Function: Adjust the indexes of the left and right child of the parent.
  
  #Iterate through the matrix, starting at the second column. This utilizes R's column major structure
  for(i in (newbtree$depth + 1):length(newbtree$tree)){
    #If the index(a number) is greater than the index of the delete row, subtract one.
    if(newbtree$tree[i] > delRow){
      newbtree$tree[i] = newbtree$tree[i] - 1
    }
    #If the index equals the deleted row and it did not indicate the smallest element has a child, set it to -1.
    else if(newbtree$tree[i] == delRow && !child){
      newbtree$tree[i] = -1
    }
  }
  return(newbtree)
}


#Print bintree is simple. If it is an empty structure, we return nothing.
#Otherwise, print the binary tree
print.bintree <- function(btree) {
  #Input: Binary tree
  #Ouput: Print the binary tree
  #Function: Print the binary tree
  if(is.na(btree$tree[1])){
    invisible(' ')
  }
  else{
    print(btree$tree)
  }
}
  
  stack = function() {
e = new.env()
s = list(
e = e,
push = function(v) {
s = c(get("s", e), v)
l = length(s)
print(l)
assign("s", s, e)
},

pop = function() {
  s = get("s", e)
  l = length(s)
  print(l)
  r = s[l];
  print(r)
  if (l == 1) {
    assign("s", list(), e)
  } else{
    assign("s", s[1:(l - 1)], e)
  }
  print(l)
  return(r)
},

print.queue = function() {
  print(get("s", e))
}

)
assign("this", s, envir = e)
class(s) = append(class(s), "queue")
return(s)
}

queue = function() {
e = new.env()
q = list(
e = e,
push = function(v) {
q = c(v, get("q", e))
l = length(q)
print(l)
assign("q", q, e)
v
},

pop = function() {
  q = get("q", e)
  l = length(q)
  print(l)
  r = q[l];
  print(r)
  if (l == 1) {
    assign("q", list(), e)
  } else{
    assign("q", q[1:(l - 1)], e)
  }
  print(l)
  return(r)
},

print.queue = function() {
  print(get("q", e))
}

)
assign("this", q, envir = e)
class(q) = append(class(q), "queue")
return(q)
}
