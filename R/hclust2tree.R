#' Transform a hclust object into a tree (list)
#'
#' @param clustering hclust object
#'
#' @include VerifyTree.R
#' @author Simon-Pierre Gadoury
#' @export

hclust2tree <- function(clustering){
  fit <- clustering
  k <- 1
  ll <- list()
  vk <- numeric(dim(fit$merge)[1])

  for (i in 1:(dim(fit$merge)[1] - 1)){
    procedure <- fit$merge[i,]
    if (procedure[1] < 0 & procedure[2] < 0){
      ll[[i]] <- list(-fit$merge[i,1], -fit$merge[i,2])
      vk[i] <- k
      k <- k + 1
    }
    else if (prod(procedure) < 0){
      vk[i] <- vk[max(procedure)]
      ll[[vk[i]]] <- list(-min(procedure), ll[[vk[i]]])
    }
    else{
      vk[i] <- vk[min(procedure)]
      ll[[vk[i]]] <- list(ll[[vk[min(procedure)]]],
                          ll[[vk[max(procedure)]]])
      ll[[vk[i]]] <- NULL
    }
  }
  ll
}
