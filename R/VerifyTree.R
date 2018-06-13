#' Verify tree structure
#'
#' @description Given a matrix of data, where the rows are observations
#' and the columns are variables, it verifies the statistical significance of
#' hierarchical nodes provided by hclust, through the use of the
#' empirical matrix of Spearman's rho.
#'
#' @details The hypothesis testing, as well as the clustering, is made with the matrix of Spearman's rho
#' for a given dataset, see \insertCite{gaisser2010testing}{ercv}.
#'
#' @param data data used for the clustering
#' @param alpha the confidence level for the tests
#' @param nboot the number of bootstrap samples to use
#' @param distance.method method for the distance matrix
#' @param hclust.method method for the clustering
#'
#' @importFrom "stats" cor quantile hclust dist
#' @author Simon-Pierre Gadoury
#' @return A list, containing the bootrap samples, the initial tree structure, modified, according to the results of the tests and a list of igraph.data.frame
#' objects. Note that the third element of the list is to keep track of the algorithm steps (the p-values are also provided).
#'
#' @examples
#' require(HAC)
#' str <- hac(type = 1, tree = list(list(list("X4", "X5", 6),
#'                                       "X6", 3), "X1", list("X2", "X3", 10), 1))
#'
#' set.seed(2018)
#' U.. <- rHAC(1000, str)
#' U.. <- U..[,c(4, 5, 6, 1, 2, 3)]
#'
#' ## Tree via hclust
#' spear <- cor(U.., method = "sp")
#' clust <- hclust(dist(spear, method = "maximum"),
#'                 method = "complete")
#' tree1 <- hclust2tree(clust)
#'
#' ## Tree after verification
#' tree2 <- VerifyTree(U.., alpha = 0.95,
#'                     distance.method = "maximum",
#'                     hclust.method = "complete")$Tree
#'
#' ## Comparison
#' par(mfrow = c(1, 3))
#' tree2plot(tree1)
#' tree2plot(tree2)
#' plot(str)
#' par(mfrow = c(1, 3))
#'
#' @references
#' \insertRef{gaisser2010testing}{erhcv}
#' @export

VerifyTree <- function(data, alpha = 0.95, nboot = 500,
                       distance.method = "maximum",
                       hclust.method = "complete"){

  spear <- cor(data, method = "sp")
  dd <- dist(spear, method = distance.method)
  tree_fit <- hclust(dd, method = hclust.method)
  tree <- hclust2tree(tree_fit)

  m <- nboot
  mm <- dim(data)[2]
  nn <- dim(data)[1]

  ## Bootstrap
  SpearmanBoot <- array(0, dim = c(mm, mm, m))
  for (i in 1:m){
    pos <- sample(1:nn, replace = T)
    SpearmanBoot[,,i] <-  cor(data[pos,], method = "sp")
  }

  ## Arrangement of the data
  k <- 1
  Names <- ""
  SpearmanBootResized <- matrix(0, ncol = (mm - 1) * mm / 2, nrow = m)
  for (i in 1:(mm - 1)){
    for (j in (i + 1):mm){
      Names <- c(Names, paste("(", i, ",", j, ")", sep = ""))
      SpearmanBootResized[,k] <- SpearmanBoot[i,j,]
      k <- k + 1
    }
  }
  Names <- Names[-1]
  colnames(SpearmanBootResized) <- Names
  SpearmanBootResized <- as.data.frame(SpearmanBootResized)

  ## Verification function
  Verif <- function(tree){
    e2 <- new.env()
    e2$k <- 1
    e2$evol <- list()
    TreeSelection <- function(tree, path = numeric(0), k = 1){

      IterativeTreeVerification <- function(tree, path){

        initialCondition <- 0
        for (element in tree){
          if (length(element) > 1){
            initialCondition <- 1
            break
          }
        }

        if (initialCondition == 1){
          e1 <- new.env()
          e1$FinalTree <- list()
          e1$k <- 1
          TreeElimination <- function(tree){

            for (i in 1:length(tree)){
              initialCondition <- 1
              # for (element in tree[[i]]){
              #   if (length(element) > 1){
              #     initialCondition <- 1
              #     break
              #   }
              if (initialCondition == 1){
                NewTree_fit <- ClusterNodeSelection(tree, i, alpha, data, SpearmanBootResized)
                if (length(NewTree_fit) == 2){
                  e2$evol[[e2$k]] <- tree2plot(NewTree_fit$cluster, plot = F)
                  names(e2$evol)[e2$k] <- NewTree_fit$pvalue
                  e2$k <- e2$k + 1
                }
                e1$FinalTree <- NewTree_fit$cluster
                if (length(e1$FinalTree) != length(tree)){
                  TreeElimination(e1$FinalTree)
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
          e2$TREE <- IterativeTreeVerification(tree, 0)

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
          eval(parse(text = paste(ini, " <- IterativeTreeVerification(tree, path)", sep = "")))
          for (i in 1:length(eval(parse(text = ini)))){
            eval(parse(text = paste("TreeSelection(", ini, "[[",i,"]], path = c(path, ",
                                  i,"), k = 2)", sep = "")))
          }
          # eval(parse(text = paste("for (i in 1:length(", ini, ")){TreeSelection(", ini, "[[",i,"]], path = c(path, ",
          #                         i,"), k = 2)}", sep = "")))
        }
      }
    }
    TreeSelection(tree)
    list("Bootstrap samples" = SpearmanBootResized,
         "Tree" = e2$TREE,
         "pvalues" = e2$evol)
  }
  Verif(tree)
}

