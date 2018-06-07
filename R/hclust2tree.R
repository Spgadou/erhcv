#' Transform a hclust object into a cluster list
#'
#' @param clustering hclust object
#'
#' @author Simon-Pierre Gadoury
#' @export

hclust2tree <- function(clustering){
  fit <- clustering
  k <- 1
  ll <- list()
  vk <- numeric(dim(fit$merge)[1])
  for (i in 1:dim(fit$merge)[1]){
    procedure <- fit$merge[i,]
    if (procedure[1] < 0 & procedure[2] < 0){
      vk[i] <- k
      k <- k + 1
    }
    else if (prod(procedure) < 0){
      vk[i] <- vk[max(procedure)]
    }
  }

  for (i in 1:(dim(fit$merge)[1] - 1)){
    if (vk[i] == i){
      ll[[i]] <- list(-fit$merge[i,1], -fit$merge[i,2])
    }
    else{
      pos <- neg <- fit$merge[i,]
      pos <- pos[pos > 0]
      neg <- neg[neg < 0]
      if (length(neg) == 0){
        ll[[vk[min(pos)]]] <- list(ll[[vk[min(pos)]]], ll[[vk[max(pos)]]])
        ll[[vk[max(pos)]]] <- NULL
      }
      else{
        ll[[vk[pos]]] <- list(-neg, ll[[vk[pos]]])
      }
    }
  }
  ll
}
