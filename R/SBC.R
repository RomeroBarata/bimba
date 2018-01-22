#' The Under-Sampling Based on Clustering algorithm.
#' 
#' \code{SBC} under-samples the input data using the Under-Sampling Based on 
#' Clustering algorithm.
#' 
#' Under-Sampling Based on Clustering clusters the input data into \emph{k} 
#' clusters and randomly selects a number of majority examples from each 
#' cluster based on the imbalance ratio of the cluster.
#' 
#' The authors did not specify if sampling of majority examples should be 
#' performed with or without replacement, however, in many occasions the 
#' algorithm tries to sample more examples than what is available in the 
#' cluster, therefore, we always perform sampling with replacement here.
#'
#' @inheritParams KMUS
#' @param k Number of clusters for the \code{k}-Means algorithm.
#' @return A data frame containing a more balanced version of the input data 
#'  after under-sampling with the Under-Sampling Based on Clustering algorithm.
#' @references Yen, S. J., & Lee, Y. S. (2009). Cluster-based under-sampling
#'  approaches for imbalanced data distributions.
#'  \emph{Expert Systems with Applications}, \emph{36}(3), 5718-5727.
#' @export

SBC <- function(data, perc_maj = 50, perc_under = NULL, k = 3, 
                max_iter = 100L, nstart = 10L, classes = NULL){
  y <- data[[ncol(data)]]
  if (is.null(classes)) classes <- extract_classes(y)
  
  maj_indices <- which(y == classes[["Majority"]])
  min_indices <- which(y == classes[["Minority"]])
  
  maj_size <- length(maj_indices)
  min_size <- length(min_indices)
  sample_size <- compute_undersample_size(majority_size = maj_size, 
                                          minority_size = min_size,
                                          perc_maj = perc_maj,
                                          perc_under = perc_under)
  if (sample_size == 0) return(data)
  
  cluster_ids <- kmeans(data[-ncol(data)], 
                        num_centers = k,
                        max_iter = max_iter, 
                        nstart = nstart)$cluster
  
  unique_ids <- seq_len(k)
  maj_cluster_ids <- factor(cluster_ids[maj_indices], levels = unique_ids)
  min_cluster_ids <- factor(cluster_ids[min_indices], levels = unique_ids)
  ratios <- table(maj_cluster_ids) / (table(min_cluster_ids) + 1)
  
  m <- (maj_size - sample_size) / min_size
  sample_size_per_cluster <- round((m * min_size) * (ratios / sum(ratios)))
  
  maj_indices_to_keep <- vector("list", length = 3L)
  for (i in seq_len(k)){
    cluster_i <- maj_cluster_ids == i
    if (!any(cluster_i)) next
    
    maj_indices_to_keep[[i]] <- sample(maj_indices[cluster_i],
                                       size = sample_size_per_cluster[i],
                                       replace = TRUE)
  }
  maj_indices_to_keep <- unlist(maj_indices_to_keep)
  
  indices_to_keep <- sort(c(maj_indices_to_keep, min_indices))
  data[indices_to_keep, , drop = FALSE]
}