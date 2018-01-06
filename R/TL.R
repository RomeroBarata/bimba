#' Tomek Links.
#' 
#' \code{TL} is a cleaning algorithm that removes examples that belong to 
#' Tomek Links. A pair of examples form a Tomek Link if they belong to 
#' different classes and are the nearest neighbours of each other.
#' 
#' The user has control over the examples that are removed and can select to 
#' remove only examples from majority class (under-sampling), minority class, 
#' and from both classes (cleaning).
#' 
#' @inheritParams ENN
#' @return A data frame containing a clean version of the input data set 
#'  after removing examples that belong to Tomek Links.
#' @references Tomek, I. (1976). An experiment with the edited 
#' nearest-neighbor rule. \emph{IEEE Transactions on systems, Man, and 
#' Cybernetics}, (6), 448-452.
#' @export

TL <- function(data, remove_class = "Majority", classes = NULL){
  y <- data[[ncol(data)]]
  if (is.null(classes)) classes <- extract_classes(y)
  
  is_tl <- identify_tomek_links(data)
  
  keep_indices <- 
    if (remove_class == "Both") !is_tl
  else !(is_tl & (y == classes[[remove_class]]))
  
  data[keep_indices, , drop = FALSE]
}

#' Identify Tomek Links.
#' 
#' \code{identify_tomek_links} returns a vector indicating whether each 
#' example in the input data set belong to a Tomek Link or not.
#' 
#' @inheritParams ENN
#' @return A logical vector indicating for each example in the input data set 
#'  whether it belongs to a Tomek Link or not.
#' @export

identify_tomek_links <- function(data){
  nn_indices <- knn(data_train = data,
                    data_test = data,
                    k = 1,
                    remove_first_neighbour = TRUE)$knn_indices[, 1]
  
  num_examples <- nrow(data)
  nn_nn_indices <- nn_indices[nn_indices]
  y <- data[, ncol(data)]
  y_nn <- data[nn_indices, ncol(data)]
  (seq_len(num_examples) == nn_nn_indices) & (y != y_nn)
}