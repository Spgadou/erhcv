#' Obtain pairs of leaves
#'
#' @description Find all the pairs of leaves between
#' a specific subcluster and all the other subclusters
#' under the main cluster. Pairs between the subclusters
#' of the specific subcluster are also given.
#'
#' @param cluster main cluster (of the form provided by the function hclust2tree)
#' @param pos position of the subcluster directly under the main cluster.
#' This subcluster is the one every other subcluster will be paired with.
#'
#' @include VerifyTree.R
#'
#' @author Simon-Pierre Gadoury
#'
#' @return Matrix of pairs of leaves.
#'
#' @export

GetPairs <- function(cluster, pos){
  nClust <- length(cluster) # Number of subclusters

  ## Pairs within the cluster under test
  MAT <- c(0, 0)
  cluster_test <- cluster[[pos]]
  stopifnot(length(cluster_test) > 1)
  for (i in 1:(length(cluster_test) - 1)){
    for (j in (i + 1):length(cluster_test)){
      L1 <- GetLeaves(cluster_test[[i]]) # Leaves of first subcluster
      L2 <- GetLeaves(cluster_test[[j]]) # Leaves of second subcluster
      MAT <- rbind(MAT, expand.grid(L1, L2)) # Pair leaves for Rho computation
    }
  }

  pos2 <- 1:nClust
  pos2 <- pos2[-pos]
  for (i in pos2){
    L1 <- GetLeaves(cluster[[i]]) # Leaves of first subcluster
    L2 <- GetLeaves(cluster_test) # Leaves of second subcluster
    MAT <- rbind(MAT, expand.grid(L1, L2))
  }

  MAT[-1,]
}
