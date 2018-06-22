#' Modifed Spearman's rho matrix
#'
#' @description Modifies, according to the tree structure provided, the empirical Spearman's rho matrix.
#'
#' @param Tree tree structure to use for the grouping
#' @param EmpSpearmanMatrix initial empirical Spearman's rho matrix to modify
#'
#' @author Simon-Pierre Gadoury
#'
#' @export

ModifySpearman <- function(Tree, EmpSpearmanMatrix){
  Spear <- EmpSpearmanMatrix
  e1 <- new.env()
  e1$Spearman <- matrix(0, ncol = dim(Spear)[1], nrow = dim(Spear)[1])
  FUN <- function(Tree){
    if (class(Tree) == "list"){
      k <- 1
      sp <- c(0)
      mat <- c(0, 0)
      for (i in 1:(length(Tree) - 1)){
        for (j in (i + 1):length(Tree)){
          mat <- rbind(mat, expand.grid(GetLeaves(Tree[[i]]),
                                        GetLeaves(Tree[[j]])))
        }
      }
      mat <- mat[-1,]
      for (i in 1:dim(mat)[1]){
        sp <- c(sp, Spear[mat[i,1], mat[i,2]])
      }
      mu <- mean(sp[-1])
      for (i in 1:dim(mat)[1]){
        e1$Spearman[min(mat[i,1], mat[i,2]),
                    max(mat[i,1], mat[i,2])] <- mu
      }
      for (element in Tree){
        FUN(element)
      }
    }
  }
  FUN(Tree)
  e1$Spearman + t(e1$Spearman) + diag(rep(1, dim(Spear)[1]))
}
