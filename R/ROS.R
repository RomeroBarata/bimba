#' The Random Over-Sampling algorithm.
#'
#' \code{ROS} returns a more balanced version of a data set after application
#' of the Random Over-Sampling algorithm.
#'
#' The Random Over-Sampling algorithm works by appending randomly selected
#' examples from the minority class (with replacement) to the original data
#' set.
#'
#' @param data A data frame containing the predictors and the outcome. The
#'  outcome must be both a binary valued factor and the last column of
#'  \code{data}.
#' @param perc_min The desired \% size of the minority class relative to the
#'  whole data set. For instance, if \code{perc_min} = 50 the returned data
#'  set is balanced. \code{perc_min} is ignored if \code{perc_over} is
#'  specified.
#' @param perc_over \% of examples to append to the input data set relative
#'  to the size of the minority class. For instance, if \code{perc_over} = 100
#'  the minority class doubles in size. If specified, \code{perc_min} is
#'  ignored.
#' @param classes A named vector identifying the majority and the minority
#'  classes. The names must be "Majority" and "Minority". This argument is
#'  only useful if the function is called inside another sampling function.
#' @return A data frame containing a more balanced version of the input data
#'  set after application of the Random Over-Sampling algorithm.
#' @examples
#' imb_data <- generate_imbalanced_data(num_examples = 200, 
#'                                      num_features = 2,
#'                                      imbalance_ratio = 5,
#'                                      noise_maj = 0,
#'                                      noise_min = 0,
#'                                      seed = 42)
#'  
#' table(imb_data$target)
#' table(ROS(imb_data, perc_min = 50)$target)    # Balance the classes
#' table(ROS(imb_data, perc_over = 100)$target)  # Double minority class size
#' @export

ROS <- function(data, perc_min = 50, perc_over = NULL, classes = NULL){
  y <- data[[ncol(data)]]
  if (is.null(classes)) classes <- extract_classes(y)

  min_idx <- which(y == classes[["Minority"]])

  min_size <- length(min_idx)
  maj_size <- nrow(data) - min_size

  sample_size <- compute_oversample_size(majority_size = maj_size,
                                         minority_size = min_size,
                                         perc_min = perc_min,
                                         perc_over = perc_over)
  if (sample_size == 0) return(data)

  # Avoid for instance sample(22, size = 30, replace = TRUE)
  if (min_size == 1) min_idx <- rep(min_idx, 2)
  append_idx <- sample(min_idx, size = sample_size, replace = TRUE)

  rbind(data, data[append_idx, , drop = FALSE])
}
