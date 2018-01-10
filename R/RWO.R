#' The Random Walk Over-Sampling algorithm.
#' 
#' \code{RWO} over-samples the input data using the Random Walk Over-Sampling 
#' algorithm.
#' 
#' Random Walk Over-Sampling is based on the Central Limit Theorem and 
#' synthesises new examples for the minority class by perturbing the 
#' available minority examples.
#'
#' @inheritParams SMOTE
#' @return A data frame containing a more balanced version of the input data 
#'  after over-sampling with the Random Walk Over-Sampling algorithm.
#' @references Zhang, H., & Li, M. (2014). RWO-Sampling: A random walk
#'  over-sampling approach to imbalanced data classification.
#'  \emph{Information Fusion}, \emph{20}, 99-116.
#' @export

RWO <- function(data, perc_min = 50, perc_over = NULL, classes = NULL){
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
  
  data_synth <- synthesise_RWO(data_min = data_min, sample_size = sample_size)
  
  rbind(data, data_synth)
}

synthesise_RWO <- function(data_min, sample_size){
  X_min <- data_min[-ncol(data_min)]
  features_sd <- vapply(X_min,
                        function(x) c(sd(x, na.rm = TRUE), sum(!is.na(x))),
                        numeric(2))
  features_sd_size <- features_sd[2, ]
  features_sd <- features_sd[1, ]
  scale_factor <- features_sd / sqrt(features_sd_size)
  
  min_size <- nrow(X_min)
  p <- synth_per_example(sample_size, min_size)
  num_features <- ncol(X_min)
  X_synth <- matrix(0, nrow = sum(p), ncol = num_features)
  
  X_min <- as.matrix(X_min)
  end_indices <- cumsum(p)
  start_indices <- end_indices - p + 1
  for (i in seq_len(min_size)){
    if (p[i] == 0) next
    
    synth <- matrix(rnorm(p[i] * num_features), nrow = num_features)
    synth <- t(synth * scale_factor)
    synth <- -sweep(synth, 2, X_min[i, ], "-")
    X_synth[start_indices[i]:end_indices[i], ] <- synth
  }
  
  data_synth <- as.data.frame(X_synth)
  data_synth <- cbind(data_synth, data_min[[ncol(data_min)]][1], 
                      row.names = NULL)
  colnames(data_synth) <- colnames(data_min)
  data_synth
}