#' The Edited Nearest Neighbours algorithm.
#' 
#' \code{ENN} removes the examples that are misclassified by their \code{k} 
#' nearest neighbours.
#' 
#' The default behaviour of \code{ENN} is to remove examples from the majority 
#' class that are misclassified by their \code{k} nearest neighbours, however, 
#' the user can modify this and select to remove only examples from the 
#' minority class or remove examples from both classes.
#' 
#' @inheritParams KMUS
#' @param remove_class Examples from \code{remove_class} are removed. The 
#'  options are: \code{c("Majority", "Minority", "Both")}.
#' @param k Number of nearest neighbours to take into account.
#' @return A data frame containing a clean version of the input data set after 
#'  application of the Edited Nearest Neighbours algorithm.
#' @references Wilson, D. L. (1972). Asymptotic properties of nearest 
#' neighbor rules using edited data. \emph{IEEE Transactions on Systems, Man, 
#' and Cybernetics}, \emph{2}(3), 408-421.
#' @export

ENN <- function(data, remove_class = "Majority", k = 3, classes = NULL){
  y <- data[[ncol(data)]]
  if (is.null(classes)) classes <- extract_classes(y)
  
  knn_classes <- knn(data_train = data,
                     data_test = data,
                     k = k,
                     remove_first_neighbour = TRUE)$knn_classes
  
  y_hat <- apply(knn_classes, 1, function(x) names(which.max(table(x))))
  
  keep_indices <- 
    if (remove_class == "Both") !(y != y_hat)
  else !((y != y_hat) & (y == classes[[remove_class]]))
  
  data[keep_indices, , drop = FALSE]
}