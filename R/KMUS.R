#' The k-Means Under-Sampling algorithm.
#' 
#' \code{KMUS} returns a more balanced version of a data set after 
#' under-sampling the majority class using the k-Means algorithm.
#' 
#' KMUS is an adaptation of the k-Means algorithm to work as an under-sampling 
#' algorithm. It clusters the majority class using the k-Means algorithm and 
#' uses the centroids computed as the representatives of the majority class.
#' 
#' @inheritParams RUS
#' @param data A data frame containing the predictors and the outcome. The 
#'  predictors must be numeric and the outcome must be both a binary valued 
#'  factor and the last column of \code{data}.
#' @param max_iter Maximum number of iterations of the k-Means algorithm.
#' @param nstart Number of random restarts of the k-Means algorithm.
#' @return A data frame containing a more balanced version of the input data 
#'  set after under-sampling it with the KMUS algorithm. As the majority 
#'  examples returned by KMUS are not examples of the input data set, original 
#'  order of the examples cannot be preserved. Thus, the returned data frame 
#'  contains all majority examples followed by all minority examples.
#' @export

KMUS <- function(data, perc_maj = 50, perc_under = NULL, max_iter = 100L, 
                 nstart = 10L, classes = NULL){
  y <- data[[ncol(data)]]
  class_levels <- levels(y)
  if (is.null(classes)) classes <- extract_classes(y)
  
  X_maj <- data[y == classes[["Majority"]], -ncol(data), drop = FALSE]
  data_min <- data[y == classes[["Minority"]], , drop = FALSE]
  
  maj_size <- nrow(X_maj)
  min_size <- nrow(data_min)
  num_centers <- maj_size - compute_undersample_size(majority_size = maj_size,
                                                     minority_size = min_size,
                                                     perc_maj = perc_maj,
                                                     perc_under = perc_under)
  if (num_centers == maj_size) return(data)
  
  X_centroids <- kmeans(X_maj, 
                        num_centers = num_centers,
                        max_iter = max_iter,
                        nstart = nstart)$centers
  
  data_centroids <- cbind(X_centroids, classes[["Majority"]])
  colnames(data_centroids) <- colnames(data)
  data <- rbind(data_centroids, data_min)
  data[[ncol(data)]] <- factor(data[[ncol(data)]], levels = class_levels)
  data
}