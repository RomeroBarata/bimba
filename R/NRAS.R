#' The NRAS algorithm.
#' 
#' \code{NRAS} removes minority examples that have the proportion of minority 
#' examples among their k nearest neighbours below a threshold.
#' 
#' NRAS fits a logistic regression model to the data and uses it to predict 
#' the probability of examples being part of the minority class. These 
#' probabilities are included as a new feature of the data and then the 
#' minority examples that have few minority examples as their neighbours 
#' are removed.
#' 
#' Note that the present implementation does not perform over-sampling with 
#' SMOTE as in the original article. Here, the cleaning was decoupled from the 
#' over-sampling to make NRAS usable with any other over-sampling algorithm. 
#' Therefore, the \code{sampling_sequence} function should be used in 
#' conjunction with \code{NRAS} to perform over-sampling using any 
#' over-sampling algorithm.
#' 
#' @inheritParams SMOTE
#' @param theshold All minority examples where the proportion of minority 
#'  neighbours is below the \code{threshold} are removed from \code{data}.
#' @return A data frame containing a cleaned version of the input data after 
#'  using the NRAS algorithm.
#' @references Rivera, W. A. (2017). Noise Reduction A Priori Synthetic 
#'  Over-Sampling for class imbalanced data sets. \emph{Information Sciences}, 
#'  \emph{408}, 146-161.
#' @export

NRAS <- function(data, k = 5, threshold = 0.50, classes = NULL){
  X <- data[-ncol(data)]
  y <- data[[ncol(data)]]
  if (is.null(classes)) classes <- extract_classes(y)
  
  lr_model <- LiblineaR::LiblineaR(data = X, target = y, type = 0)
  model_preds <- predict(lr_model, X, proba = TRUE)
  model_preds <- model_preds$probabilities[, classes[["Minority"]]]
  
  data <- cbind(min_probs = model_preds, data)
  min_indices <- which(y == classes[["Minority"]])
  
  knn_classes <- knn(data_train = data,
                     data_test = data[min_indices, , drop = FALSE],
                     k = k,
                     remove_first_neighbour = TRUE)$knn_classes
  
  min_neighbours_proportions <- rowMeans(knn_classes == classes[["Minority"]])
  indices_to_remove <- min_indices[min_neighbours_proportions < threshold]
  
  if (length(min_indices) == length(indices_to_remove)){
    warning("All minority examples were removed as threshold value was ", 
            "too high. Returning the original input data.")
    return(data[-1])
  }
  
  if (length(indices_to_remove) > 0)
    data <- data[-indices_to_remove, , drop = FALSE]
  else
    warning("No minority examples were removed by NRAS. Consider increasing ",
            "the threshold value.")
  
  data[-1]
}