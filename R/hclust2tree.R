#' hclust object transformation
#'
#' @description Transforms a hclust object into a tree (cluster) used
#' in other functions of this package.
#'
#' @param clustering hclust object
#'
#' @include VerifyTree.R
#'
#' @author Simon-Pierre Gadoury
#'
#' @export

hclust2tree <- function(clustering){
  fit <- clustering

  for (i in 1:dim(fit$merge)[1]){
    procedure <- fit$merge[i,]
    if (procedure[1] < 0 & procedure[2] < 0){
      eval(parse(text = paste("L", i, " <- list(-fit$merge[", i, ",1], -fit$merge[", i, ",2])", sep = "")))
    }
    else if (prod(procedure) < 0){
      toMerge <- -min(procedure)
      eval(parse(text = paste("L", i, " <- list(toMerge, L", max(procedure), ")", sep = "")))
    }
    else{
      eval(parse(text = paste("L", i, " <- list(L", procedure[1], ",L", procedure[2], ")", sep = "")))
    }
  }
  eval(parse(text = paste("L", i, sep = "")))
}
