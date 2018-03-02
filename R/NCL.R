#' The Neighbourhood Cleaning Rule algorithm.
#' 
#' \code{NCL} removes examples from majority class that are either 
#' misclassified by their \code{k} nearest neighbours or contributed to the 
#' misclassification of examples from minority class.
#' 
#' @inheritParams ENN
#' @return A data frame containing a clean version of the input data set after 
#'  application of the Neighbourhood Cleaning Rule algorithm.
#' @references Laurikkala, J. (2001, July). Improving identification of 
#'  difficult small classes by balancing class distribution. In 
#'  \emph{Conference on Artificial Intelligence in Medicine in Europe} (pp. 
#'  63-66). Springer, Berlin, Heidelberg.
#' @export

NCL <- function(data, k = 3, classes = NULL){
  y <- data[[ncol(data)]]
  if (is.null(classes)) classes <- extract_classes(y)
  
  knn_info <- knn(data_train = data,
                  data_test = data,
                  k = k,
                  remove_first_neighbour = TRUE)
  knn_indices <- knn_info$knn_indices
  knn_classes <- knn_info$knn_classes
  
  y_hat <- apply(knn_classes, 1, function(x) names(which.max(table(x))))
  
  is_minority <- y == classes[["Minority"]]
  is_majority <- y == classes[["Majority"]]
  is_misclassified <- y != y_hat
  is_misclassified_min <- is_minority & is_misclassified
  is_misclassified_maj <- is_majority & is_misclassified
  
  offenders_maj <- 
    knn_classes[is_misclassified_min, , drop = FALSE] == classes[["Majority"]]
  offender_indices <- knn_indices[is_misclassified_min, , drop = FALSE]
  offender_indices <- unique(offender_indices[offenders_maj])
  
  is_offender_maj <- (1:nrow(data)) %in% offender_indices
  
  data[!(is_misclassified_maj | is_offender_maj), , drop = FALSE]
}