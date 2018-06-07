#' Tree to graph dataframe
#'
#' @param tree the tree under consideration
#'
#' @author Simon-Pierre Gadoury
#' @return a graph.data.frame object
#' @import igraph
#' @export

tree2graphdf <- function(tree){
  e1 <- new.env()
  e1$MAT <- c(0, 0)
  FUN <- function(tree, k = 0){
    for (i in 1:length(tree)){
      if (class(tree[[i]]) == "list"){
        e1$MAT <- rbind(e1$MAT,
                        c(paste("(", paste(k, collapse = ","), ")", sep = ""),
                          paste("(", paste(k, collapse = ","), ",", i, ")", sep = "")))
      }
      else{
        e1$MAT <- rbind(e1$MAT,
                        c(paste("(", paste(k, collapse = ","), ")", sep = ""),
                          tree[[i]]))
      }

      if (length(tree[[i]]) > 1){
        FUN(tree[[i]], c(k, i))
      }
    }
  }
  FUN(tree)
  e1$MAT <- as.data.frame(e1$MAT[-1,])
  colnames(e1$MAT) <- c("parent", "node")
  g <- igraph::graph.data.frame(e1$MAT)
  g
}
