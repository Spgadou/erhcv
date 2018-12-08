#' Plot of a tree structure
#'
#' @description Provide the plot and/or the data.tree object for a given tree structure.
#'
#' @param tree the tree under consideration (of the form provided by the function hclust2tree)
#' @param structure logical. Should the data.tree structure be returned?
#' @author Simon-Pierre Gadoury
#'
#' @return a plot and/or data.tree object
#'
#' @import data.tree
#'
#' @export

tree2plot <- function(tree, structure = FALSE){
  e1 <- new.env()
  e1$O <- data.tree::Node$new("(O)")

  Update_tree <- function(tree, k = "O"){
    if (length(tree) > 1){
      for (i in 1:length(tree)){
        if (length(tree[[i]]) > 1){
          expr0 <- strsplit(k, "")[[1]]
          expr <- paste("(", paste(c(expr0, i), collapse = ","), ")", sep = "")
          expr2 <- paste(paste("e1$", k, i, sep = ""),
                         " <- e1$",
                         k,
                         "$AddChild('", expr, "')",
                         sep = "")
          eval(parse(text = expr2))
          Update_tree(tree[[i]], k = paste(k, i, sep = ""))
        }
        else{
          expr2 <- paste(paste("e1$", k, i, "l", sep = ""),
                         " <- e1$",
                         k,
                         "$AddChild('", tree[[i]], "')",
                         sep = "")
          eval(parse(text = expr2))
        }
      }
    }
  }
  Update_tree(tree)
  if (structure){
    list("tree" = e1$O,
         " " = plot(e1$O))
  }
  else
    plot(e1$O)
}
