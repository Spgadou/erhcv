#' Leaves extractions
#'
#' @description Extract the leaves (nodes in the bottom of the hierarchy) under
#' (not necessarily directly under) a cluster.
#'
#' @param cluster a cluster (of the form provided by the function hclust2tree)
#'
#' @include VerifyTree.R
#'
#' @author Simon-Pierre Gadoury
#'
#' @return Vector of leaves.
#'
#' @export

GetLeaves <- function(cluster){
  e1 <- new.env()
  e1$Leaves <- 0

  FUN <- function(tree){
    for (i in 1:length(tree)){
      if (class(tree[[i]]) == "list"){
        FUN(tree[[i]])
      }
      else{
        e1$Leaves <- c(e1$Leaves, tree[[i]])
      }
    }
  }

  FUN(cluster)
  sort(e1$Leaves[-1])
}
