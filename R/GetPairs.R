#' Get pairs of leaves from different branches of a cluster
#'
#' @param cluster main cluster.
#' @param pos position of the subcluster directly under the main cluster.
#' This cluster is the one every other cluster will be paired with.
#'
#' @author Simon-Pierre Gadoury
#' @return Matrice of pairs of leaves.
#' @export

GetPairs <- function(cluster, pos){
  nClust <- length(cluster) # Number of subclusters

  ## Pairs within the cluster under test
  cluster_test <- cluster[[pos]]
  L1 <- GetLeaves(cluster[[pos]][[1]]) # Leaves of first subcluster
  L2 <- GetLeaves(cluster[[pos]][[2]]) # Leaves of second subcluster
  MAT <- expand.grid(L1, L2) # Pair leaves for Rho computation

  pos2 <- 1:nClust
  pos2 <- pos2[-pos]
  for (i in pos2){
    L1 <- GetLeaves(cluster[[i]]) # Leaves of first subcluster
    L2 <- GetLeaves(cluster[[pos]]) # Leaves of second subcluster
    MAT <- rbind(MAT, expand.grid(L1, L2))
  }

  MAT
}
