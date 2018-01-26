#' The MWMOTE algorithm.
#' 
#' \code{MWMOTE} over-samples the input data using the Majority Weighted 
#' Over-Sampling TEchnique.
#' 
#' MWMOTE is a complex over-sampling algorithm and comprises three main 
#' phases. First, the hard-to-learn minority examples are identified, then 
#' an importance weight is assigned to each of the hard-to-learn examples, and 
#' finally new examples are synthesised following a strategy similar to SMOTE.
#' 
#' For clarity, the hyperparameters Cf(th), CMAX, and Cp in the original 
#' description of MWMOTE were renamed here to \code{cut_off}, 
#' \code{max_closeness}, and \code{cluster_complexity}, respectively.
#'
#' @inheritParams SMOTE
#' @param k1 Number of neighbours used to identify noisy minority examples.
#' @param k2 Number of neighbours used to identify the borderline majority 
#'  examples.
#' @param k3 Number of neighbours used to identify the informative minority 
#'  examples.
#' @param cut_off Cut-off value to compute the closeness factor.
#' @param max_closeness Maximum value for the closeness factor.
#' @param cluster_complexity Value utilised to tune the trade-off between the 
#'  number of clusters and their size. A large value leads to larger clusters 
#'  but fewer of them, whereas a small value leads to smaller clusters but 
#'  more of them.
#' @return A data frame containing a more balanced version of the input data 
#'  after over-sampling with the MWMOTE algorithm.
#' @references Barua, S., Islam, M. M., Yao, X., & Murase, K. (2014). 
#'  MWMOTE--majority weighted minority oversampling technique for imbalanced 
#'  data set learning. \emph{IEEE Transactions on Knowledge and Data 
#'  Engineering}, \emph{26}(2), 405-425.
#' @export

MWMOTE <- function(data, perc_min = 50, perc_over = NULL, 
                   k1 = 5, k2 = 3, k3 = round(nrow(data_min_filtered) / 2), 
                   cut_off = 5, max_closeness = 2, cluster_complexity = 3, 
                   classes = NULL){
  y <- data[[ncol(data)]]
  if (is.null(classes)) classes <- extract_classes(y)
  
  data_maj <- data[y == classes[["Majority"]], , drop = FALSE]
  data_min <- data[y == classes[["Minority"]], , drop = FALSE]
  
  maj_size <- nrow(data_maj)
  min_size <- nrow(data_min)
  
  sample_size <- compute_oversample_size(majority_size = maj_size,
                                         minority_size = min_size,
                                         perc_min = perc_min,
                                         perc_over = perc_over)
  if (sample_size == 0) return(data)
  
  # Identify and filter noisy minority examples, i.e. examples where all of 
  # their k1 nearest neighbours are majority examples
  k1nn_classes <- knn(data_train = data,
                      data_test = data_min,
                      k = k1, 
                      remove_first_neighbour = TRUE)$knn_classes
  
  filtered_indices <- rowSums(k1nn_classes == classes[["Minority"]]) > 0
  data_min_filtered <- data_min[filtered_indices, , drop = FALSE]
  
  k2nn_indices <- knn(data_train = data_maj,
                      data_test = data_min_filtered,
                      k = k2,
                      remove_first_neighbour = FALSE)$knn_indices
  
  maj_borderline_indices <- unique(as.integer(k2nn_indices))
  data_maj_borderline <- data_maj[maj_borderline_indices, , drop = FALSE]
  
  k3nn_indices <- knn(data_train = data_min_filtered,
                      data_test = data_maj_borderline,
                      k = k3,
                      remove_first_neighbour = FALSE)$knn_indices
  
  min_informative_indices <- unique(as.integer(k3nn_indices))
  data_min_informative <- 
    data_min_filtered[min_informative_indices, , drop = FALSE]
  
  closeness_factor <- compute_closeness_factor(data_maj_borderline, 
                                               data_min_filtered, 
                                               min_informative_indices,
                                               cf = cut_off,
                                               cmax = max_closeness,
                                               k3nn_indices = k3nn_indices)
  density_factor <- closeness_factor / rowSums(closeness_factor)
  selection_weights <- colSums(closeness_factor * density_factor)
  
  cluster_ids <- hierarchical_clusters(data_min_filtered = data_min_filtered, 
                                       cp = cluster_complexity)
  
  data_synth <- 
    synthesise_MWMOTE(data_min_filtered = data_min_filtered,
                      min_informative_indices = min_informative_indices,
                      informative_weights = selection_weights,
                      filtered_cluster_ids = cluster_ids,
                      sample_size = sample_size)
  
  rbind(data, data_synth)
}

compute_closeness_factor <- function(data_maj_borderline, 
                                     data_min_filtered, 
                                     min_informative_indices,
                                     cf, cmax, k3nn_indices){
  X_maj_borderline <- data_maj_borderline[-ncol(data_maj_borderline)]
  X_maj_borderline <- as.matrix(X_maj_borderline)
  X_min_filtered <- data_min_filtered[-ncol(data_min_filtered)]
  X_min_filtered <- as.matrix(X_min_filtered)
  
  # Not 100% sure about how standardization of the data sets should be 
  # carried out in this case, need to think more about this.
  # For now, this one seems to be a reasonable solution.
  X_maj_borderline <- center_and_scale(X_maj_borderline)
  X_min_filtered <- scale(X_min_filtered,
                          center = attr(X_maj_borderline, "scaled:center"),
                          scale = attr(X_maj_borderline, "scaled:scale"))
  
  maj_borderline_size <- nrow(X_maj_borderline)
  min_filtered_size <- nrow(X_min_filtered)
  closeness_factor <- matrix(0, 
                             nrow = maj_borderline_size,
                             ncol = min_filtered_size)
  
  num_features <- ncol(X_maj_borderline)
  for (i in seq_len(maj_borderline_size)){
    nns_indices <- k3nn_indices[i, ]
    X_min_nns <- X_min_filtered[nns_indices, , drop = FALSE]
    distances <- euclidian_distance(X_min_nns, X_maj_borderline[i, ])
    closeness_factor[i, nns_indices] <- num_features / distances
  }
  closeness_factor <- closeness_factor[, min_informative_indices, drop = FALSE]
  
  closeness_factor <- ifelse(closeness_factor > cf, cf, closeness_factor)
  (closeness_factor / cf) * cmax
}

hierarchical_clusters <- function(data_min_filtered, cp){
  X_min_filtered <- data_min_filtered[-ncol(data_min_filtered)]
  X_min_filtered_standardized <- center_and_scale(X_min_filtered)
  distances <- stats::dist(X_min_filtered_standardized)
  hclust_model <- stats::hclust(distances, method = "average")
  distances <- as.matrix(distances)
  diag(distances) <- Inf
  davg <- (1 / nrow(distances)) * sum(apply(distances, 1, min))
  height_threshold <- davg * cp
  stats::cutree(hclust_model, h = height_threshold)
}

synthesise_MWMOTE <- function(data_min_filtered, min_informative_indices,
                              informative_weights, filtered_cluster_ids,
                              sample_size){
  X_min_filtered <- data_min_filtered[-ncol(data_min_filtered)]
  
  selected_informative_indices <- sample(min_informative_indices,
                                         size = sample_size,
                                         replace = TRUE,
                                         prob = informative_weights)
  X_synth <- X_min_filtered[selected_informative_indices, , drop = FALSE]
  X_synth <- as.matrix(X_synth)
  
  selected_informative_cluster_ids <- 
    filtered_cluster_ids[selected_informative_indices]
  
  selected_ids_distribution <- table(selected_informative_cluster_ids)
  unique_selected_ids <- as.integer(names(selected_ids_distribution))
  
  split_data <- split(X_min_filtered, filtered_cluster_ids)
  X_cluster_neighbours <- vector("list", length = length(unique_selected_ids))
  for (i in seq_along(unique_selected_ids)){
    X <- split_data[[unique_selected_ids[i]]]
    cluster_sample_size <- selected_ids_distribution[[i]]
    indices <- sample(seq_len(nrow(X)), size = cluster_sample_size, 
                      replace = TRUE)
    X_cluster_neighbours[[i]] <- X[indices, , drop = FALSE]
  }
  X_cluster_neighbours <- do.call(rbind, X_cluster_neighbours)
  
  deltas <- runif(sample_size)
  X_synth <- X_synth[order(selected_informative_cluster_ids), , drop = FALSE]
  
  X_synth <- X_synth + deltas * (as.matrix(X_cluster_neighbours) - X_synth)
  
  data_synth <- as.data.frame(X_synth)
  data_synth <- cbind(data_synth, 
                      data_min_filtered[[ncol(data_min_filtered)]][1], 
                      row.names = NULL)
  colnames(data_synth) <- colnames(data_min_filtered)
  data_synth
}