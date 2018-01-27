#' Chain sampling algorithms together.
#' 
#' \code{sampling_sequence} applies a sequence of sampling algorithms to an 
#' input data set.
#' 
#' \code{sampling_sequence} has two main arguments: \code{algorithms} and 
#' \code{parameters}. \code{algorithms} is a vector containing the names of 
#' the sampling functions to be applied in sequence (either as strings or 
#' the functions' objects themselves). \code{parameters} is a list of lists, 
#' where each individual list contains the parameters to be used by the 
#' respective sampling function in \code{algorithms}.
#' 
#' @inheritParams ROS
#' @param data A data frame containing the predictors and the outcome. 
#'  Restrictions about the input data depend on the individual sampling 
#'  algorithms being used. In any case, the outcome must be both a binary 
#'  valued factor and the last column of \code{data}.
#' @param algorithms A vector containing the names of the sampling algorithms 
#'  to be chained either as strings or the functions' objects themselves.
#' @param parameters A list of lists where each individual list contains the 
#'  parameters to be used by the respective sampling algorithm in 
#'  \code{algorithms}.
#' @return A data frame containing the result of a chain of sampling 
#'  algorithms applied to the input data set.
#' @export

sampling_sequence <- function(data, algorithms, parameters = NULL, 
                              classes = NULL){
  y <- data[[ncol(data)]]
  if (is.null(classes)) classes <- extract_classes(y)
  
  if (is.null(parameters)){
    parameters <- vector("list", length = length(algorithms))
    for (i in seq_along(parameters)) parameters[[i]] <- list()
  }
  
  for (i in seq_along(algorithms)){
    data <- do.call(algorithms[i],
                    args = c(list(data = data), 
                             parameters[[i]],
                             list(classes = classes)))
  }
  
  data
}