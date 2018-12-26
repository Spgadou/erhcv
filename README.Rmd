[Homepage](https://spgadou.github.io)

### CRAN

[https://cran.r-project.org/web/packages/erhcv/index.html](https://cran.r-project.org/web/packages/erhcv/index.html)

### Short description

Assesses the statistical significance of clusters for a given dataset through bootstrapping and hypothesis testing of a given matrix of empirical Spearman's rho, based on the technique of S. Gaiser et al. (2010). 

### Tree structure convention

In this package, a *tree structure* consists of nested lists, which is very natural. To explain the concept, let us use the following custom data.tree object:
```r
1  (O)            
2   ¦--(O,1)      
3   ¦   ¦--(O,1,1)
4   ¦   ¦   ¦--7  
5   ¦   ¦   °--8  
6   ¦   ¦--6      
7   ¦   ¦--5      
8   ¦   °--(O,1,4)
9   ¦       ¦--9  
10  ¦       °--10 
11  ¦--2          
12  ¦--1          
13  °--(O,4)      
14      ¦--3      
15      °--4    
```
The associated *tree structure* is then constructed as follows.
```r
treeStructure <- list(list(list(7, 8), 6, 5, list(9, 10)), 2, 1, list(3, 4))
```
Note that one can transform this tree into a data.tree object using tree2plot.
```r
tree2plot(treeStructure, plot = FALSE) # data.tree
```
As a general guideline, a tree structure needs to be a list. Then, one distinguishes nodes from leafs by object types. Indeed, nodes are lists, and leafs are integers. The construction is analogous to the structure construction of the package [nCopula](https://cran.r-project.org/web/packages/nCopula/index.html).

### Main functions

Function | Description
----------| -------------
VerifyTree | Main function of the package, used to *clean* raw hclust clustering
hclust2tree | Transforms a hclust object into a tree structure (under the convention of this package)
tree2plot | Illustration of a tree (or data.tree structure)

## Working example

### Packages

For a proper demonstration of the package, we use the package *nCopula* to sample hierarchical data.

```r
install.packages(erhcv); install.packages(nCopula)
```

### Dataset sampling and clustering

```r
library(nCopula)

## Build structure
structure <- GEO(0.5, 1:2, list(GAMMA(1/2, 3:4, NULL),
                                GEO(0.3, 5:6, list(GAMMA(1/3, 7:8, NULL),
                                                   GAMMA(1/3, 9:10, NULL)))))
                                                   
## Sample from the structure
set.seed(123)
U.. <- rCompCop(1000, structure)

## Compute Spearman correlation matrix
Spearman <- cor(U.., method = "sp")

## Cluster Spearman matrix
distance <- dist(Spearman, method = "maximum")
clustering <- hclust(distance, method = "average")

## Transform clustering into nested lists
tree <- erhcv::hclust2tree(clustering)
erhcv::tree2plot(tree, structure = TRUE) # data.tree object

1  (O)                        
2   ¦--(O,1)                  
3   ¦   ¦--(O,1,1)            
4   ¦   ¦   ¦--7              
5   ¦   ¦   °--8              
6   ¦   °--(O,1,2)            
7   ¦       ¦--6              
8   ¦       °--(O,1,2,2)      
9   ¦           ¦--5          
10  ¦           °--(O,1,2,2,2)
11  ¦               ¦--9      
12  ¦               °--10     
13  °--(O,2)                  
14      ¦--2                  
15      °--(O,2,2)            
16          ¦--1              
17          °--(O,2,2,2)      
18              ¦--3          
19              °--4  
```

We rapidly see that the obtained structure is far from the original one. We then use tools from *erhcv* to eliminate unnecessary nodes, based on our (subjective) simplification level *alpha*.

### Clustering validation

Here, we make use of *VerifyTree* to chop down nodes of the clustering we obtained earlier. 

```r
## Clean the tree
alpha <- 1 # Severe simplification parameter
cleanedTree <- erhcv::VerifyTree(U..,
                                 alpha = alpha,
                                 distance.method = "maximum",
                                 hclust.method = "average")$Tree

## Visualize output
erhcv::tree2plot(cleanedTree, structure = TRUE)

1  (O)            
2   ¦--(O,1)      
3   ¦   ¦--(O,1,1)
4   ¦   ¦   ¦--7  
5   ¦   ¦   °--8  
6   ¦   ¦--6      
7   ¦   ¦--5      
8   ¦   °--(O,1,4)
9   ¦       ¦--9  
10  ¦       °--10 
11  ¦--2          
12  ¦--1          
13  °--(O,4)      
14      ¦--3      
15      °--4 
```

Bellow is the associated tree structure.

![Verified tree structure](/TrueTree.png)
