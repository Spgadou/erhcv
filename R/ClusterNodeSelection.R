#' Subcluster significance test
#'
#' @description Test the significance of a subcluster directly under a given cluster.
#' If it is not significant, the subcluster is eliminated and
#' its element are merged in the main cluster. Bootstrap samples of the underlying
#' data needs to be provided, as well as the structure under consideration.
#'
#' @details The hypothesis testing is made with the matrix of Spearman's rho
#' for a given dataset, see \insertCite{gaisser2010testing}{ercv}.
#'
#' @param cluster the main cluster (of the form provided by the function hclust2tree)
#' @param testPos the position of the subcluster to test, directly under the main cluster
#' @param alpha the confidence level for the tests
#' @param data the underlying data
#' @param BootData the dataframe of bootstrap samples of Spearman rho, with columns
#' named "(i,j)", where "i" and "j" are different leaves
#'
#' @include VerifyTree.R
#'
#' @import utils
#' @import stringr
#' @import stringi
#' @importFrom Rdpack reprompt
#'
#' @author Simon-Pierre Gadoury
#'
#' @return The main cluster, with or without the node under test, wether the
#' hypothesis can be rejected or not.
#'
#' @references
#' \insertRef{gaisser2010testing}{erhcv}
#' @export

ClusterNodeSelection <- function(cluster, testPos, alpha, data, BootData){

  if (length(cluster[[testPos]]) != 1){
    BootData_UnderTest <- NULL

    MAT <- GetPairs(cluster, testPos)
    spear <- cor(data, method = "sp")

    n <- dim(data)[1]
    nn <- dim(BootData)[1]
    m <- dim(MAT)[1]

    spear_calc <- 0
    for (i in 1:dim(MAT)[1]){
      spear_calc <- c(spear_calc, spear[MAT[i,1], MAT[i,2]])
    }
    spear_calc <- spear_calc[spear_calc != 1][-1] # Get sampled Rho (no boot)
    ini <- "BootData_UnderTest <- cbind(z)"
    input <- "BootData$`(z1,z2)`"
    res1 <- numeric(dim(MAT)[1])
    for (i in 1:dim(MAT)[1]){
      test <- input
      test <- stringr::str_replace_all(test, 'z1', as.character(min(MAT[i,1], MAT[i,2])))
      test <- stringr::str_replace_all(test, 'z2', as.character(max(MAT[i,1], MAT[i,2])))
      res1[i] <- test
    }
    ini <- stringr::str_replace_all(ini, 'z', paste(res1, collapse = ", "))
    eval(parse(text = ini)) # Extract bootstrapped Rho

    T2_FNB <- matrix(0, ncol = m, nrow = nn)
    for (i in 1:nn){
      T2_FNB[i,] <- c(cbind(BootData_UnderTest[i,]) - c(rbind(rep(1, m)) %*% cbind(BootData_UnderTest[i,]) / m) * cbind(rep(1, m)))
    }

    T2_FN <- c(cbind(spear_calc) - c(rbind(rep(1, m)) %*% cbind(spear_calc) / m) * cbind(rep(1, m)))
    T2_FN <- matrix(T2_FN, ncol = m, nrow = nn, byrow = T)

    MAT <- (T2_FNB - T2_FN) * sqrt(n)
    CritVal_dist <- apply(MAT, 1, function(x) sum(x^2) / m)
    K <- quantile(CritVal_dist, alpha)
    Q <- sum((spear_calc - mean(spear_calc))^2) * (n / m)

    if (Q < K){
      EliminateCluster(cluster, testPos)
    }
    else{
      cluster
    }
  }
  else{
    cluster
  }
}
