#' The ADASYN algorithm.
#' 
#' \code{ADASYN} over-samples the input data using the Adaptive Synthetic 
#' Sampling algorithm.
#' 
#' ADASYN is an adaptation of the SMOTE algorithm which focuses on 
#' synthesising more examples for the minority examples that are considered 
#' "hard" to learn. The learning hardness of a minority example is defined as 
#' being proportional to the number of majority examples among the \code{k} 
#' nearest neighbours of the minority example. There are two cases where 
#' no examples are synthesised for a minority example. The first case is when 
#' all \code{k} nearest neighbours belong to the majority class and the 
#' minority examples is considered to be noise. The second case is when all 
#' \code{k} nearest neighbours belong to the minority class and the minority 
#' example is considered too easy to learn (learning hardness = 0).
#' 
#' Compared to ADASYN's original description, the current implementation has 
#' a few differences. Firstly, the \eqn{d_{th}} parameter was dropped.
#' Secondly, the \eqn{\beta} parameter was replaced by \code{perc_min} and 
#' \code{perc_over} parameters. The modification allows the user to synthesise
#' as many examples as wanted and \eqn{\beta = 1} is equivalent to
#' \code{perc_min} = 50 (balance the distribution of examples).
#' 
#' @inheritParams SMOTE
#' @return A data frame containing a more balanced version of the input data 
#'  set after over-sampling it with ADASYN.
#' @references He, H., Bai, Y., Garcia, E. A., & Li, S. (2008, June). ADASYN:
#'  Adaptive synthetic sampling approach for imbalanced learning. In
#'  \emph{Neural Networks, 2008. IJCNN 2008.(IEEE World Congress on
#'  Computational Intelligence). IEEE International Joint Conference on}
#'  (pp. 1322-1328). IEEE.
#' @export

ADASYN <- function(data, perc_min = 50, perc_over = NULL, 
                   k = 5, classes = NULL){
  y <- data[[ncol(data)]]
  if (is.null(classes)) classes <- extract_classes(y)
  
  data_min <- data[y == classes[["Minority"]], , drop = FALSE]
  min_size <- nrow(data_min)
  maj_size <- nrow(data) - min_size
  
  sample_size <- compute_oversample_size(majority_size = maj_size,
                                         minority_size = min_size,
                                         perc_min = perc_min,
                                         perc_over = perc_over)
  if (sample_size == 0) return(data)
  
  knn_info <- knn(data_train = data,
                  data_test = data_min,
                  k = k,
                  remove_first_neighbour = TRUE)
  
  knn_classes <- knn_info$knn_classes
  knn_maj_ratio <- rowMeans(knn_classes == classes[["Majority"]])
  min_noise_indices <- knn_maj_ratio == 1
  if (sum(knn_maj_ratio) == 0) return(data)
  
  knn_maj_ratio <- knn_maj_ratio / sum(knn_maj_ratio)
  knn_maj_ratio[min_noise_indices] <- 0
  data_synth <- synthesise_ADASYN(data_min = data_min,
                                  data = data,
                                  sample_size = sample_size,
                                  knn_info = knn_info,
                                  knn_maj_ratio = knn_maj_ratio)
  
  rbind(data, data_synth)
}

synthesise_ADASYN <- function(data_min, data, sample_size,
                              knn_info, knn_maj_ratio){
  X_min <- as.matrix(data_min[-ncol(data_min)])
  X_data <- as.matrix(data[-ncol(data)])
  
  p <- round(knn_maj_ratio * sample_size)
  num_features <- ncol(X_min)
  X_synth <- matrix(0, nrow = sum(p), ncol = num_features)
  
  knn_indices <- knn_info$knn_indices
  k <- ncol(knn_indices)
  knn_classes <- knn_info$knn_classes
  knn_is_min <- knn_classes == (data_min[1, ncol(data_min)])
  end_indices <- cumsum(p)
  start_indices <- end_indices - p + 1
  deltas <- runif(nrow(X_synth))
  for (i in seq_len(nrow(X_min))){
    if (p[i] == 0) next
    
    selected_nns <- sample(1:k, size = p[i], replace = TRUE, 
                           prob = knn_is_min[i, ])
    nns_indices <- knn_indices[i, selected_nns]
    X_nns <- X_data[nns_indices, , drop = FALSE]
    synth <- sweep(X_nns, 2, X_min[i, ], "-")
    synth <- synth * deltas[start_indices[i]:end_indices[i]]
    synth <- sweep(synth, 2, X_min[i, ], "+")
    X_synth[start_indices[i]:end_indices[i], ] <- synth
  }
  
  data_synth <- as.data.frame(X_synth)
  data_synth <- cbind(data_synth, data_min[[ncol(data_min)]][1], 
                      row.names = NULL)
  colnames(data_synth) <- colnames(data_min)
  data_synth
}