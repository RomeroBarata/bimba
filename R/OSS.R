#' The One-Sided Selection algorithm.
#' 
#' \code{OSS} under-samples the input data using the One-Sided Selection 
#' algorithm.
#' 
#' OSS first reduces the original data set into a consistent subset and then 
#' removes all majority examples that belong to Tomek Links. To find a 
#' consistent subset, OSS creates a subset of the data containing a random 
#' example from majority class and all examples from minority class, and adds 
#' to this subset all majority examples that are missclassified by this subset 
#' using the 1-NN rule.
#' 
#' @inheritParams KMUS
#' @return A data frame containing a more balanced version of the input data 
#'  after under-sampling it with OSS.
#' @references Kubat, M., & Matwin, S. (1997, July). Addressing the curse of 
#'  imbalanced training sets: one-sided selection. In \emph{ICML} (Vol. 97, 
#'  pp. 179-186).
#' @export

OSS <- function(data, classes = NULL){
  y <- data[[ncol(data)]]
  if (is.null(classes)) classes <- extract_classes(y)
  
  maj_indices <- which(y == classes[["Majority"]])
  min_indices <- which(y == classes[["Minority"]])
  
  maj_idx <- sample(maj_indices, size = 1)
  nn_classes <- knn(data_train = data[c(maj_idx, min_indices), , drop = FALSE],
                    data_test = data[maj_indices, , drop = FALSE],
                    k = 1,
                    remove_first_neighbour = FALSE)$knn_classes[, 1]
  
  missclassified_indices <- maj_indices[nn_classes != y[maj_indices]]
  indices_to_keep <- c(min_indices, maj_idx, missclassified_indices)
  indices_to_remove <- setdiff(seq_len(nrow(data)), indices_to_keep)
  
  if (length(indices_to_remove) > 0) 
    data <- data[-indices_to_remove, , drop = FALSE]
  
  TL(data, remove_class = "Majority", classes = classes)
}