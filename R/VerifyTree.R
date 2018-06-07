#' Verify tree structure
#'
#' @param tree the main tree
#' @param data the data used to construct the tree
#' @param alpha the confidence level for the tests
#' @param nboot the number of bootstrap samples to use
#'
#' @importFrom "stats" cor quantile
#' @author Simon-Pierre Gadoury
#' @return The tree, modified, according to the results of the tests.
#' @export

VerifyTree <- function(tree, data, alpha = 0.95, nboot = 500){

  m <- nboot
  mm <- dim(data)[2]
  SpearmanBoot <- array(0, dim = c(mm, mm, m))
  for (i in 1:m){
    pos <- sample(1:dim(data)[1], replace = T)
    SpearmanBoot[,,i] <- cor(data[pos,], method = "sp")
  }

  k <- 1
  SpearmanBootResized <- matrix(0, ncol = (mm - 1) * mm / 2, nrow = m)
  for (i in 1:(mm - 1)){
    for (j in (i + 1):mm){
      SpearmanBootResized[,k] <- SpearmanBoot[i,j,]
      k <- k + 1
    }
  }
  Names <- ""
  for (i in 1:(mm - 1)){
    for (j in (i + 1):mm){
      Names <- c(Names, paste("(", i, ",", j, ")", sep = ""))
    }
  }
  Names <- Names[-1]
  colnames(SpearmanBootResized) <- Names
  SpearmanBootResized <- as.data.frame(SpearmanBootResized)

  FUN <- function(tree){
    e2 <- new.env()
    e2$k <- 1
    TreeSelection <- function(tree, path = numeric(0), k = 1){

      IterativeTreeVerification <- function(tree){

        initialCondition <- 0
        for (element in tree){
          if (length(element) > 1){
            initialCondition <- 1
          }
        }

        if (initialCondition == 1){
          e1 <- new.env()
          e1$FinalTree <- list()
          e1$k <- 1
          TreeElimination <- function(tree){

            for (i in 1:length(tree)){
              initialCondition <- 0
              for (element in tree[[i]]){
                if (length(element) > 1){
                  initialCondition <- 1
                }
              }
              if (initialCondition == 1){
                NewTree <- ClusterNodeSelection(tree, i, alpha, data, SpearmanBootResized)
                e1$FinalTree <- NewTree
                if (length(NewTree) != length(tree)){
                  TreeElimination(NewTree)
                  break
                }
              }
              else{
                e1$FinalTree <- tree
              }
            }
          }
          TreeElimination(tree)
          e1$FinalTree
        }
        else{
          tree
        }
      }

      if (length(tree) != 1){
        if (k == 1){
          e2$TREE <- IterativeTreeVerification(tree)

          for (i in 1:length(e2$TREE)){
            TreeSelection(e2$TREE[[i]], path = c(path, i), k = 2)
          }
        }
        else{
          res1 <- numeric(length(path))
          for (i in 1:length(path)){
            res1[i] <- paste("[[", path[i], "]]", sep = "")
          }
          ini <- paste(res1, collapse = "")
          ini <- paste("e2$TREE", ini, sep = "")
          eval(parse(text = paste(ini, " <- IterativeTreeVerification(tree)", sep = "")))
          eval(parse(text = paste("for (i in 1:length(", ini, ")){TreeSelection(", ini, "[[",i,"]], path = c(path, ",
                                  i,"), k = 2)}", sep = "")))
        }
      }
    }
    TreeSelection(tree)
    list("Bootstrap samples" = SpearmanBootResized,
         "Tree" = e2$TREE)
  }

  FUN(tree)
}
