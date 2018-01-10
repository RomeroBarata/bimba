#' The borderline-SMOTE algorithm.
#' 
#' \code{BDLSMOTE} over-samples the input data using either borderline-SMOTE1 
#' or borderline-SMOTE2 algorithms.
#' 
#' Borderline-SMOTE\{1, 2\} algorithms work similarly to SMOTE, however, they 
#' only synthesise new examples using the minority examples that are 
#' borderline.
#' 
#' @inheritParams SMOTE
#' @param m Number of neighbours used to decide whether a minority example 
#'  is borderline or not. The authors call the set of all borderline examples 
#'  the DANGER set.
#' @param borderline Select between borderline-SMOTE1 and borderline-SMOTE2. 
#'  Possible values are 1 and 2.
#' @return A data frame containing a more balanced version of the input data 
#'  after over-sampling with either borderline-SMOTE1 or borderline-SMOTE2.
#' @references Han, H., Wang, W. Y., & Mao, B. H. (2005, August).
#'  Borderline-SMOTE: a new over-sampling method in imbalanced data sets
#'  learning. In \emph{International Conference on Intelligent Computing}
#'  (pp. 878-887). Springer Berlin Heidelberg.
#' @export

BDLSMOTE <- function(data, perc_min = 50, perc_over = NULL, 
                     over_replace = FALSE, k = 5, m = 2 * (k + 1), 
                     borderline = 1, classes = NULL){
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
  
  mnn_info <- knn(data_train = data,
                  data_test = data_min,
                  k = m,
                  remove_first_neighbour = TRUE)
  
  mnn_classes <- mnn_info$knn_classes
  num_maj_neighbours <- rowSums(mnn_classes == classes[["Majority"]])
  danger_indices <- (num_maj_neighbours >= (m / 2)) & (num_maj_neighbours < m)
  if (!any(danger_indices)) return(data)
  
  data_danger <- data_min[danger_indices, , drop = FALSE]
  data_synth <- 
    if (borderline == 1){
      knn_indices <- knn(data_train = data_min,
                         data_test = data_danger,
                         k = k,
                         remove_first_neighbour = TRUE)$knn_indices
      synthesise_BDLSMOTE1(data_danger = data_danger,
                           data_min = data_min,
                           sample_size = sample_size,
                           knn_indices = knn_indices,
                           over_replace = over_replace)
    } else{
      knn_info <- 
        if (k <= m){
          mnn_indices <- mnn_info$knn_indices
          mnn_classes <- mnn_info$knn_classes
          list(knn_indices = mnn_indices[danger_indices, 1:k, drop = FALSE], 
               knn_classes = mnn_classes[danger_indices, 1:k, drop = FALSE])
        } else{
          knn(data_train = data,
              data_test = data_danger,
              k = k,
              remove_first_neighbour = TRUE)
        }
      synthesise_BDLSMOTE2(data_danger = data_danger,
                           data = data,
                           sample_size = sample_size,
                           knn_info = knn_info,
                           over_replace = over_replace,
                           maj_class = classes[["Majority"]])
    }
  
  rbind(data, data_synth)
}

synthesise_BDLSMOTE1 <- function(data_danger, data_min, sample_size, 
                                 knn_indices, over_replace){
  X_danger <- as.matrix(data_danger[-ncol(data_danger)])
  X_min <- as.matrix(data_min[-ncol(data_min)])
  
  danger_size <- nrow(X_danger)
  p <- synth_per_example(sample_size, danger_size)
  X_synth <- matrix(0, nrow = sum(p), ncol = ncol(X_danger))
  
  k <- ncol(knn_indices)
  over_replace <- if (over_replace) rep(over_replace, danger_size) else p > k
  
  end_indices <- cumsum(p)
  start_indices <- end_indices - p + 1
  deltas <- runif(nrow(X_synth), min = 0, max = 1)
  for (i in seq_len(danger_size)){
    if (p[i] == 0) next
    
    selected_nns <- sample(1:k, size = p[i], replace = over_replace[i])
    nns_indices <- knn_indices[i, selected_nns]
    X_nns <- X_min[nns_indices, , drop = FALSE]
    synth <- sweep(X_nns, 2, X_danger[i, ], "-")
    synth <- synth * deltas[start_indices[i]:end_indices[i]]
    synth <- sweep(synth, 2, X_danger[i, ], "+")
    X_synth[start_indices[i]:end_indices[i], ] <- synth
  }
  
  data_synth <- as.data.frame(X_synth)
  data_synth <- cbind(data_synth, data_danger[[ncol(data_danger)]][1], 
                      row.names = NULL)
  colnames(data_synth) <- colnames(data_danger)
  data_synth
}

synthesise_BDLSMOTE2 <- function(data_danger, data, sample_size,
                                 knn_info, over_replace, maj_class){
  X_danger <- as.matrix(data_danger[-ncol(data_danger)])
  X_data <- as.matrix(data[-ncol(data)])
  
  danger_size <- nrow(X_danger)
  p <- synth_per_example(sample_size, danger_size)
  X_synth <- matrix(0, nrow = sum(p), ncol = ncol(X_danger))
  
  knn_indices <- knn_info$knn_indices
  knn_classes <- knn_info$knn_classes
  k <- ncol(knn_indices)
  over_replace <- if (over_replace) rep(over_replace, danger_size) else p > k
  
  end_indices <- cumsum(p)
  start_indices <- end_indices - p + 1
  for (i in seq_len(danger_size)){
    if (p[i] == 0) next
    
    selected_nns <- sample(1:k, size = p[i], replace = over_replace[i])
    nns_indices <- knn_indices[i, selected_nns]
    nns_classes <- knn_classes[i, selected_nns]
    max_flags <- nns_classes == maj_class
    deltas <- runif(p[i], min = 0, max = ifelse(max_flags, 0.5, 1.0))
    X_nns <- X_data[nns_indices, , drop = FALSE]
    synth <- sweep(X_nns, 2, X_danger[i, ], "-")
    synth <- synth * deltas
    synth <- sweep(synth, 2, X_danger[i, ], "+")
    X_synth[start_indices[i]:end_indices[i], ] <- synth
  }
  
  data_synth <- as.data.frame(X_synth)
  data_synth <- cbind(data_synth, data_danger[[ncol(data_danger)]][1], 
                      row.names = NULL)
  colnames(data_synth) <- colnames(data_danger)
  data_synth
}