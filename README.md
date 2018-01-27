# bimba
## Overview
The `bimba` package implements a variety of sampling algorithms to reduce the imbalance present in many real world data sets. Although multi-class imbalanced data sets are common, `bimba` has been designed to work only with two-class imbalanced data sets.

`bimba`'s main goal is to be **flexible** as its main use is for research purposes. In addition, as many over-sampling and under-sampling algorithms have a similar structure and several common hyperparameters, a lot of care was taken to ensure consistency between different functions, making `bimba` **intuitive** to use.

## Installation
`bimba` is under active development and not yet available on CRAN. The 
development version can be installed as follows:

```r
# install.packages("devtools")
devtools::install_github("RomeroBarata/bimba")
```

## Quick Tour

```r
library(bimba)


```

## Available Algorithms
Many over-sampling, under-sampling, and hybrid algorithms are available. In addition, the algorithms can be easily chained using the `sampling_sequence` function. A complete list of the algorithms, broken down by their type, is available below.

### Over-Sampling
- `ADASYN`: Adaptive Synthetic Sampling [5]
- `BDLSMOTE`: borderline-SMOTE1 and borderline-SMOTE2 [4]
- `MWMOTE`: Majority Weighted Minority Over-Sampling TEchnique [8]
- `ROS`: Random Over-Sampling
- `RWO`: Random Walk Over-Sampling [9]
- `SLSMOTE`: Safe-Level-SMOTE [6]
- `SMOTE`: Synthetic Minority Over-Sampling TEchnique [3]

### Under-Sampling
- `ENN`: Edited Nearest Neighbours [1]
- `KMUS`: _k_-Means Under-Sampling
- `RUS`: Random Under-Sampling
- `SBC`: Under-Sampling Based on Clustering [7]
- `TL`: Tomek Links [2]

### Cleaning
- `NRAS`: Noise Reduction A Priori Synthetic Over-Sampling [10]

To make `NRAS` more general its cleaning step has been decoupled from the over-sampling step.

### Misc
- `sampling_sequence`: Convenience function to chain sampling algorithms 
together

## Related Packages
Although several other packages implement sampling algorithms they differ to `bimba` in a few ways. Below is a non-exhaustive list of related packages 
broken down by languages.

### Python

### R

## References
[1] Wilson, D. L. (1972). **Asymptotic properties of nearest neighbor rules 
using edited data**. _IEEE Transactions on Systems, Man, and Cybernetics_, 
_2_(3), 408-421.

[2] Tomek, I. (1976). **An experiment with the edited nearest-neighbor rule**. 
_IEEE Transactions on systems, Man, and Cybernetics_, (6), 448-452.

[3] Chawla, N. V., Bowyer, K. W., Hall, L. O., & Kegelmeyer, W. P. (2002). 
**SMOTE: synthetic minority over-sampling technique**. _Journal of artificial intelligence research_, _16_, 321-357.

[4] Han, H., Wang, W. Y., & Mao, B. H. (2005, August). **Borderline-SMOTE: a 
new over-sampling method in imbalanced data sets learning**. In _International Conference on Intelligent Computing_ (pp. 878-887). Springer Berlin Heidelberg.

[5] He, H., Bai, Y., Garcia, E. A., & Li, S. (2008, June). **ADASYN: Adaptive synthetic sampling approach for imbalanced learning**. In _Neural Networks, 
2008. IJCNN 2008.(IEEE World Congress on Computational Intelligence). IEEE International Joint Conference on_ (pp. 1322-1328). IEEE.

[6] Bunkhumpornpat, C., Sinapiromsaran, K., & Lursinsap, C. (2009). **Safe-level-smote: Safe-level-synthetic minority over-sampling technique for handling the class imbalanced problem**. _Advances in knowledge discovery and data mining_, 475-482.

[7] Yen, S. J., & Lee, Y. S. (2009). **Cluster-based under-sampling approaches for imbalanced data distributions**. _Expert Systems with Applications_, 
_36_(3), 5718-5727.

[8] Barua, S., Islam, M. M., Yao, X., & Murase, K. (2014). **MWMOTE--majority 
weighted minority oversampling technique for imbalanced data set learning**. 
_IEEE Transactions on Knowledge and Data Engineering_, _26_(2), 405-425.

[9] Zhang, H., & Li, M. (2014). **RWO-Sampling: A random walk over-sampling approach to imbalanced data classification**. _Information Fusion_, _20_, 
99-116.

[10] Rivera, W. A. (2017). **Noise Reduction A Priori Synthetic Over-Sampling 
for class imbalanced data sets**. _Information Sciences_, _408_, 146-161.