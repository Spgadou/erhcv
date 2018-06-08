#' Subcluster elimination
#'
#' @description Eliminate a subcluster and merge its elements to the main cluster.
#'
#' @param cluster the cluster under consideration (of the form provided by the function hclust2tree)
#' @param pos the position of the subcluster, directly under the main cluster, to eliminate
#'
#' @include ClusterNodeSelection.R
#'
#' @author Simon-Pierre Gadoury
#'
#' @return The main cluster, without the subcluster that was eliminated.
#'
#' @export

EliminateCluster <- function(cluster, pos){
  stopifnot(length(cluster) > 1)
  FinalCluster <- list()
  if (pos > 1){
    for (i in 1:(pos - 1)){
      FinalCluster[[i]] <- cluster[[i]]
    }
    for (i in pos:(pos + length(cluster[[pos]]) - 1)){
      FinalCluster[[i]] <- cluster[[pos]][[i - pos + 1]]
    }
    if (pos < length(cluster)){
      for (i in (pos + 1):length(cluster)){
        FinalCluster[[length(cluster[[pos]]) + i - pos]] <- cluster[[i]]
      }
    }
  }
  else{
    for (i in 1:length(cluster[[1]])){
      FinalCluster[[i]] <- cluster[[1]][[i]]
    }
    for (i in 2:length(cluster)){
      FinalCluster[[length(cluster[[1]]) + i - 1]] <- cluster[[i]]
    }
  }
  FinalCluster
}
