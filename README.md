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
                                                   GAMMA(1/3, 9:10, NULL)))))
                                                   
## Sample from the structure
U.. <- rCompCop(1000, structure)

## Compute Spearman correlation matrix
Spearman <- cor(U.., method = "sp")

## Cluster Spearman matrix
distance <- dist(Spearman, method = "maximum")
clustering <- hclust(distance, method = "average")

## Transform clustering into nested lists
tree <- erhcv::hclust2tree(clustering)
tree2plot(tree, structure = TRUE) # data.tree object

1  (O)                              
2   ¦--(O,1)                        
3   ¦   ¦--1                        
4   ¦   °--(O,1,2)                  
5   ¦       ¦--3                    
6   ¦       °--4                    
7   °--(O,2)                        
8       ¦--2                        
9       °--(O,2,2)                  
10          ¦--(O,2,2,1)            
11          ¦   ¦--7                
12          ¦   °--8                
13          °--(O,2,2,2)            
14              ¦--5                
15              °--(O,2,2,2,2)      
16                  ¦--6            
17                  °--(O,2,2,2,2,2)
18                      ¦--9        
19                      °--10   
```

We rapidly see that the obtained structure is far from the original one. We then use tools from *erhcv* to eliminate unnecessary nodes, based on our (subjective) simplification level *alpha*.

```{r}
## Clean the tree
alpha <- 1 # Severe simplification parameter
cleanedTree <- VerifyTree(U.., alpha = alpha,
                          distance.method = "maximum", hclust.method = "average")$Tree

## Visualize output
tree2plot(cleanedTree, structure = TRUE)

1  (O)            
2   ¦--1          
3   ¦--(O,2)      
4   ¦   ¦--3      
5   ¦   °--4      
6   ¦--2          
7   °--(O,4)      
8       ¦--(O,4,1)
9       ¦   ¦--7  
10      ¦   °--8  
11      ¦--5      
12      ¦--6      
13      °--(O,4,4)
14          ¦--9  
15          °--10 
```


