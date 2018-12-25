# erhcv: Equi-Rank Hierarchical Clustering Validation

Assesses the statistical significance of clusters for a given dataset through bootstrapping and hypothesis testing of a given matrix of empirical Spearman's rho, based on the technique of S. Gaiser et al. (2010). 

## Working example

### Pre-requisite

For a proper demonstration of the package, we use the package *nCopula* to sample hierarchical data.

```{r}
install.packages(erhcv); install.packages(nCopula)
```

### Dataset sampling

```{r}
library(nCopula)

## Build structure
structure <- GEO(0.5, 1:2, list(GAMMA(1/2, 3:4, NULL),
                                GEO(0.3, 5:6, list(GAMMA(1/3, 7:8, NULL),
                                                   GAMMA(1/3, 9:10, NULL))))
                                                   
## Sample from the structure
U.. <- rCompCop(1000, structure)

## Compute Spearman correlation matrix
Spearman <- cor(U.., method = "sp")

## Cluster Spearman matrix
distance <- dist(Spearman, method = "maximum")
clustering <- hclust(distance, method = "average")

## Transform clustering into nested lists
tree <- erhcv::hclust2tree(clustering)
tree2plot(tree, structure = TRUE)
```

