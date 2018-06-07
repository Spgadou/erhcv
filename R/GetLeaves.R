#' Get a vector of leaves under a given cluster
#'
#' @param cluster a cluster
#'
#' @include VerifyTree.R
#' @author Simon-Pierre Gadoury
#' @return Vector of leaves under the cluster.
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
