#' Plot of a tree structure
#'
#' @param tree the tree under consideration (of the form provided by the function hclust2tree)
#' @param plot show a basic plot the tree ?
#' @param ... extra parameters passed to igraph.plot
#'
#' @author Simon-Pierre Gadoury
#' @return a plot or graph.data.frame object
#' @import igraph
#' @export

tree2plot <- function(tree, plot = TRUE, ...){
  e1 <- new.env()
  e1$MAT <- c(0, 0)
  FUN <- function(tree, k = 0){
    pos <- 1
    for (i in 1:length(tree)){
      if (class(tree[[i]]) == "list"){
        e1$MAT <- rbind(e1$MAT,
                        c(paste("(", paste(k, collapse = ","), ")", sep = ""),
                          paste("(", paste(k, collapse = ","), ",", pos, ")", sep = "")))
      }
      else{
        e1$MAT <- rbind(e1$MAT,
                        c(paste("(", paste(k, collapse = ","), ")", sep = ""),
                          tree[[i]]))
      }

      if (length(tree[[i]]) > 1){
        FUN(tree[[i]], c(k, pos))
        pos <- pos + 1
      }
    }
  }
  FUN(tree)
  e1$MAT <- as.data.frame(e1$MAT[-1,])
  colnames(e1$MAT) <- c("parent", "node")
  g <- igraph::graph.data.frame(e1$MAT)
  if (plot)
    plot(g, layout=layout.reingold.tilford,
         edge.arrow.size=0, ...)
  else
    g
}
