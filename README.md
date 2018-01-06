# bimba
## Overview
The `bimba` package implements a variety of sampling algorithms to reduce the imbalance present in many real world data sets. Although multi-class imbalanced data sets are common, `bimba` has been designed to work only with two-class imbalanced data sets.

`bimba` is easy to learn and easy to use as all algorithms implemented share a similar structure.

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
- `ROS`: Random Over-Sampling
- `SMOTE`: Synthetic Minority Over-Sampling TEchnique [2]

### Under-Sampling
- `ENN`: Edited Nearest Neighbours [1]
- `KMUS`: _k_-Means Under-Sampling
- `RUS`: Random Under-Sampling

## Related Packages
Although several other packages implement sampling algorithms they differ to `bimba` in a few ways. Below is a non-exhaustive list of related packages 
broken down by languages.

### Python

### R

## References
[1] Wilson, D. L. (1972). **Asymptotic properties of nearest neighbor rules 
using edited data**. _IEEE Transactions on Systems, Man, and Cybernetics_, 
_2_(3), 408-421.

[2] Chawla, N. V., Bowyer, K. W., Hall, L. O., & Kegelmeyer, W. P. (2002). 
**SMOTE: synthetic minority over-sampling technique**. _Journal of artificial intelligence research_, _16_, 321-357.