#' The SMOTE algorithm.
#' 
#' \code{SMOTE} returns a more balanced version of a data set after 
#' application of the SMOTE algorithm.
#' 
#' SMOTE is an over-sampling algorithm that synthesises new examples in the 
#' line segment joining two close minority class examples.
#' 
#' @inheritParams ROS
#' @param data A data frame containing the predictors and the outcome. The 
#'  predictors must be numeric and the outcome must be both a binary valued 
#'  factor and the last column of \code{data}.
#' @param k Number of nearest neighbours to compute for each example in the 
#'  minority class.
#' @param over_replace A logical value indicating whether the neighbours 
#'  picked from the \code{k} nearest neighbours should be picked with or 
#'  without replacement.
#' @return A data frame containing a more balanced version of the input data 
#'  set after application of the SMOTE algorithm.
#' @references Chawla, N. V., Bowyer, K. W., Hall, L. O., & Kegelmeyer, W. P.
#'  (2002). SMOTE: synthetic minority over-sampling technique. \emph{Journal
#'  of artificial intelligence research}, \emph{16}, 321-357.
#' @export

SMOTE <- function(data, perc_min = 50, perc_over = NULL, k = 5, 
                  over_replace = FALSE, classes = NULL){
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
  
  knn_indices <- knn(data_train = data_min,
                     data_test = data_min,
                     k = k,
                     remove_first_neighbour = TRUE)$knn_indices
  
  data_synth <- synthesise_SMOTE(data_min = data_min,
                                 sample_size = sample_size,
                                 knn_indices = knn_indices,
                                 over_replace = over_replace)
  
  rbind(data, data_synth)
}

synthesise_SMOTE <- function(data_min, sample_size, knn_indices, over_replace){
  X_min <- as.matrix(data_min[-ncol(data_min)])
  min_size <- nrow(X_min)
  num_features <- ncol(X_min)
  k <- ncol(knn_indices)
  
  p <- synth_per_example(sample_size, min_size)
  sample_size <- sum(p)
  
  over_replace <- if (over_replace) rep(over_replace, min_size) else p > k
  
  end_indices <- cumsum(p)
  start_indices <- end_indices - p + 1
  
  X_synth <- matrix(0, nrow = sample_size, ncol = num_features)
  deltas <- runif(sample_size, min = 0, max = 1)
  
  for (i in seq_len(min_size)){
    if (p[i] == 0) next
    
    selected_nns <- sample(1:k, size = p[i], replace = over_replace[i])
    nns_indices <- knn_indices[i, selected_nns]
    X_nns <- X_min[nns_indices, , drop = FALSE]
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