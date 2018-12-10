#' Compare two tree structures
#'
#' @description Provide a logical answer to the question "Are two trees equivalent?".
#'
#' @param tree_test (testing tree) a tree structure in the form of nested lists (as the output of hclust2tree, for example)
#' @param tree_ref (reference tree) a tree structure in the form of nested lists (as the output of hclust2tree, for example)

#' @author Simon-Pierre Gadoury
#'
#' @details This test is particularly useful when two trees are not ordered in the same way.
#' As the structure are lists, one could argue that this function is a modification
#' of the \code{\link{identical}} base function, overcoming the ordering.
#'
#' @return boolean: TRUE implies that the two trees are equivalent.
#'
#' @examples
#' ## Comparison between "identical" and "compareTrees"
#' ##
#' ## The trees are "identical"
#'
#' tree1 <- list(list(list(5, 6), list(7, 8), 3, 4), list(9, 10), 2, 1)
#' tree2 <- list(list(list(5, 6), list(7, 8), 3, 4), list(9, 10), 2, 1)
#'
#' CompareTrees(tree1, tree2)
#' identical(tree1, tree2)
#'
#' ## The trees are "equivalent" (notice the leaves 1 and 2 interchanged)
#'
#' tree1 <- list(list(list(5, 6), list(7, 8), 3, 4), list(9, 10), 2, 1)
#' tree2 <- list(list(list(5, 6), list(7, 8), 3, 4), list(9, 10), 1, 2)
#'
#' CompareTrees(tree1, tree2)
#' identical(tree1, tree2)
#'
#' @export

CompareTrees <- function(tree_test, tree_ref){
  e1 <- new.env()
  e1$fail <- FALSE
  f <- function(tree1, tree2){
    if (length(tree1) > 1){
      leaves1 <- sapply(tree1, erhcv::GetLeaves)
      leaves2 <- sapply(tree2, erhcv::GetLeaves)
      n1 <- length(leaves1)
      if (n1 != length(leaves2)){
        e1$fail <- TRUE
        return("")
      }
      test <- sapply(leaves1, function(x){
        list(x) %in% leaves2
      })
      if (sum(test) != n1){
        e1$fail <- TRUE
        return("")
      }
      jj <- numeric(n1)
      for (i in 1:n1){
        jj[i] <- which(sapply(1:n1, function(j){
          identical(leaves1[[i]], leaves2[[j]])
        }) == 1)
      }
      for (i in 1:n1){
        if (e1$fail == FALSE)
          f(tree1[[i]], tree2[[jj[i]]])
      }
    }
  }

  f(tree_test, tree_ref)
  if (e1$fail) FALSE else TRUE
}
