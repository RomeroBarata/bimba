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
## Some nice setup for the graphics
library(ggplot2)
clean_theme <- theme_minimal() + theme(axis.title.x = element_blank(),
                                       axis.title.y = element_blank())
                                       
## bimba in action
library(bimba)
sample_data <- generate_imbalanced_data(num_examples = 200L,
                                        imbalance_ratio = 10,
                                        noise_maj = 0,
                                        noise_min = 0.04,
                                        seed = 42)
ggplot(sample_data, aes(x = V1, y = V2, colour = target)) + 
  geom_point(size = 2) + clean_theme

# Balance the distribution of examples using SMOTE
smoted_data <- SMOTE(sample_data, perc_min = 50, k = 5)
# Sanity check. Did it really balance?
table(smoted_data$target)
ggplot(smoted_data, aes(x = V1, y = V2, colour = target)) + 
  geom_point(size = 2) + clean_theme

# SMOTE is not robust to noisy minority examples. Lets add a cleaning step 
# to the minority class before using SMOTE.
ssed_data <- sampling_sequence(sample_data, algorithms = c("NRAS", "SMOTE"))
ggplot(ssed_data, aes(x = V1, y = V2, colour = target)) + 
  geom_point(size = 2) + clean_theme

# Clean using ENN, double the size of the minority class using SMOTE, and 
# balance the distribution using RUS.
algorithms <- c("ENN", "SMOTE", "RUS")
parameters <- list(
  ENN = list(remove_class = "Minority", k = 3),
  SMOTE = list(perc_over = 100, k = 5),
  RUS = list(perc_maj = 50)
)

ssed2_data <- sampling_sequence(sample_data, algorithms = algorithms, 
                                parameters = parameters)
ggplot(ssed2_data, aes(x = V1, y = V2, colour = target)) + 
  geom_point(size = 2) + clean_theme
```

## Available Algorithms
Many over-sampling, under-sampling, and hybrid algorithms are available. In addition, the algorithms can be easily chained using the `sampling_sequence` function. A complete list of the algorithms, broken down by their type, is available below.

### Over-Sampling
- `ADASYN`: Adaptive Synthetic Sampling [7]
- `BDLSMOTE`: borderline-SMOTE1 and borderline-SMOTE2 [6]
- `MWMOTE`: Majority Weighted Minority Over-Sampling TEchnique [10]
- `ROS`: Random Over-Sampling
- `RWO`: Random Walk Over-Sampling [11]
- `SLSMOTE`: Safe-Level-SMOTE [8]
- `SMOTE`: Synthetic Minority Over-Sampling TEchnique [5]

### Under-Sampling
- `ENN`: Edited Nearest Neighbours [1]
- `KMUS`: _k_-Means Under-Sampling
- `NCL`: Neighbourhood Cleaning Rule [4]
- `OSS`: One-Sided Selection [3]
- `RUS`: Random Under-Sampling
- `SBC`: Under-Sampling Based on Clustering [9]
- `TL`: Tomek Links [2]

### Cleaning
- `NRAS`: Noise Reduction A Priori Synthetic Over-Sampling [12]

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

[3] Kubat, M., & Matwin, S. (1997, July). **Addressing the curse of imbalanced training sets: one-sided selection**. In _ICML_ (Vol. 97, pp. 179-186).

[4] Laurikkala, J. (2001, July). **Improving identification of difficult small classes by balancing class distribution**. In _Conference on Artificial Intelligence in Medicine in Europe_ (pp. 63-66). Springer, Berlin, Heidelberg.

[5] Chawla, N. V., Bowyer, K. W., Hall, L. O., & Kegelmeyer, W. P. (2002). 
**SMOTE: synthetic minority over-sampling technique**. _Journal of artificial intelligence research_, _16_, 321-357.

[6] Han, H., Wang, W. Y., & Mao, B. H. (2005, August). **Borderline-SMOTE: a 
new over-sampling method in imbalanced data sets learning**. In _International Conference on Intelligent Computing_ (pp. 878-887). Springer Berlin Heidelberg.

[7] He, H., Bai, Y., Garcia, E. A., & Li, S. (2008, June). **ADASYN: Adaptive synthetic sampling approach for imbalanced learning**. In _Neural Networks, 
2008. IJCNN 2008.(IEEE World Congress on Computational Intelligence). IEEE International Joint Conference on_ (pp. 1322-1328). IEEE.

[8] Bunkhumpornpat, C., Sinapiromsaran, K., & Lursinsap, C. (2009). **Safe-level-smote: Safe-level-synthetic minority over-sampling technique for handling the class imbalanced problem**. _Advances in knowledge discovery and data mining_, 475-482.

[9] Yen, S. J., & Lee, Y. S. (2009). **Cluster-based under-sampling approaches for imbalanced data distributions**. _Expert Systems with Applications_, 
_36_(3), 5718-5727.

[10] Barua, S., Islam, M. M., Yao, X., & Murase, K. (2014). **MWMOTE--majority 
weighted minority oversampling technique for imbalanced data set learning**. 
_IEEE Transactions on Knowledge and Data Engineering_, _26_(2), 405-425.

[11] Zhang, H., & Li, M. (2014). **RWO-Sampling: A random walk over-sampling approach to imbalanced data classification**. _Information Fusion_, _20_, 
99-116.

[12] Rivera, W. A. (2017). **Noise Reduction A Priori Synthetic Over-Sampling 
for class imbalanced data sets**. _Information Sciences_, _408_, 146-161.