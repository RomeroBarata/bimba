#' The Random Under-Sampling algorithm.
#'
#' \code{RUS} returns a more balanced version of a data set after application
#' of the Random Under-Sampling algorithm.
#'
#' The Random Under-Sampling algorithm creates a new data set containing all 
#' examples from the minority class plus a random selection of examples from 
#' the majority class.
#' 
#' @inheritParams ROS
#' @param perc_maj The desired \% size of the majority class relative to the
#'  whole data set. For instance, if \code{perc_maj} = 50 a balanced version
#'  of the input data set is returned. \code{perc_maj} is ignored if
#'  \code{perc_under} is specified.
#' @param perc_under \% of examples to select from the majority class. If
#'  specified \code{perc_maj} is ignored.
#' @return A data frame containing a more balanced version of the input data
#'  set after application of the Random Under-Sampling algorithm. The original
#'  order of the examples is preserved.
#' @examples
#' imb_data <- generate_imbalanced_data(num_examples = 200, 
#'                                      num_features = 2,
#'                                      imbalance_ratio = 5,
#'                                      noise_maj = 0,
#'                                      noise_min = 0,
#'                                      seed = 42)
#'  
#' table(imb_data$target)
#' table(RUS(imb_data, perc_maj = 50)$target)    # Balance the classes
#' table(RUS(imb_data, perc_under = 20)$target)  # Select 20% of maj. class
#' @export

RUS <- function(data, perc_maj = 50, perc_under = NULL, classes = NULL){
  y <- data[[ncol(data)]]
  if (is.null(classes)) classes <- extract_classes(y)

  maj_idx <- which(y == classes[["Majority"]])

  maj_size <- length(maj_idx)
  min_size <- nrow(data) - maj_size

  sample_size <- compute_undersample_size(majority_size = maj_size,
                                          minority_size = min_size,
                                          perc_maj = perc_maj,
                                          perc_under = perc_under)
  if (sample_size == 0) return(data)

  maj_idx_remove <- sample(maj_idx, size = sample_size, replace = FALSE)

  data[-maj_idx_remove, , drop = FALSE]
}
