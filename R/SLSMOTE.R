#' The Safe-Level-SMOTE algorithm.
#' 
#' \code{SLSMOTE} over-samples the input data set using the Safe-Level-SMOTE 
#' algorithm.
#' 
#' Safe-Level-SMOTE works similarly to SMOTE. The difference is that 
#' Safe-Level-SMOTE associates a "level of safeness" to each minority example 
#' and uses this when synthesising new examples. The level of safeness of a 
#' minority example is defined as the number of minority examples among the 
#' \code{k} nearest neighbours of the example.
#' 
#' There are several rules used by Safe-Level-SMOTE to synthesise new 
#' examples. Apart from the situation where both minority examples have safe 
#' levels of zero, and no example is synthesised, the algorithm tends to 
#' synthesise new examples closer to the minority examples with safer levels.
#' 
#' @inheritParams SMOTE
#' @return A data frame containing a more balanced version of the input data 
#'  after over-sampling with the Safe-Level-SMOTE algorithm.
#' @references Bunkhumpornpat, C., Sinapiromsaran, K., & Lursinsap, C. (2009). 
#'  Safe-level-smote: Safe-level-synthetic minority over-sampling technique 
#'  for handling the class imbalanced problem. \emph{Advances in knowledge 
#'  discovery and data mining}, 475-482.
#' @export

SLSMOTE <- function(data, perc_min = 50, perc_over = NULL, k = 5, 
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
  
  knn_classes <- knn(data_train = data,
                     data_test = data_min,
                     k = k,
                     remove_first_neighbour = TRUE)$knn_classes
  min_safe_levels <- rowSums(knn_classes == classes[["Minority"]])
  
  knn_indices <- knn(data_train = data_min,
                     data_test = data_min,
                     k = k,
                     remove_first_neighbour = TRUE)$knn_indices
  knn_safe_levels <- min_safe_levels[as.integer(knn_indices)]
  knn_safe_levels <- matrix(knn_safe_levels, ncol = k)
  knn_safe_neighbours <- knn_safe_levels > 0
  has_safe_neighbours <- rowSums(knn_safe_neighbours) > 0
  
  safe_levels <- min_safe_levels[has_safe_neighbours]
  data_safe <- data_min[has_safe_neighbours, , drop = FALSE]
  knn_indices <- knn_indices[has_safe_neighbours, , drop = FALSE]
  knn_probs <- knn_safe_neighbours[has_safe_neighbours, , drop = FALSE]
  
  data_synth <- synthesise_SLSMOTE(data_safe = data_safe,
                                   data_min = data_min,
                                   sample_size = sample_size, 
                                   knn_indices = knn_indices,
                                   knn_probs = knn_probs,
                                   safe_levels = safe_levels,
                                   min_safe_levels = min_safe_levels,
                                   over_replace = over_replace)
  
  rbind(data, data_synth)
}

synthesise_SLSMOTE <- function(data_safe, data_min, sample_size, knn_indices,
                               knn_probs, safe_levels, min_safe_levels, 
                               over_replace){
  X_safe <- as.matrix(data_safe[-ncol(data_safe)])
  X_min <- as.matrix(data_min[-ncol(data_min)])
  
  safe_size <- nrow(X_safe)
  p <- synth_per_example(sample_size, safe_size)
  X_synth <- matrix(0, nrow = sum(p), ncol = ncol(X_safe))
  
  k <- ncol(knn_indices)
  over_replace <- 
    if (over_replace) rep(over_replace, safe_size) 
  else p > rowSums(knn_probs)
  
  end_indices <- cumsum(p)
  start_indices <- end_indices - p + 1
  for (i in seq_len(safe_size)){
    if (p[i] == 0) next
    
    selected_nns <- sample(1:k, size = p[i], replace = over_replace[i], 
                           prob = knn_probs[i, ])
    nns_indices <- knn_indices[i, selected_nns]
    sl_ratio <- safe_levels[i] / min_safe_levels[nns_indices]
    deltas <- runif(p[i], 
                    min = ifelse(sl_ratio < 1, 1 - sl_ratio, 0),
                    max = ifelse(sl_ratio < 1, 1, 1 / sl_ratio))
    X_nns <- X_min[nns_indices, , drop = FALSE]
    synth <- sweep(X_nns, 2, X_safe[i, ], "-")
    synth <- synth * deltas
    synth <- sweep(synth, 2, X_safe[i, ], "+")
    X_synth[start_indices[i]:end_indices[i], ] <- synth
  }
  
  data_synth <- as.data.frame(X_synth)
  data_synth <- cbind(data_synth, data_safe[[ncol(data_safe)]][1], 
                      row.names = NULL)
  colnames(data_synth) <- colnames(data_safe)
  data_synth
}