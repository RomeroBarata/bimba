#' Generate an imbalanced data set.
#' 
#' \code{generate_imbalanced_data} is a simple function to generate a 
#' two-class imbalanced data set.
#' 
#' The imbalanced data set generated has two classes where the majority class 
#' comes from a multivariate normal distribution with mean zero and unitary 
#' standard deviation for all features and the minority class comes from a 
#' multivariate normal distribution with mean two and unitary standard 
#' deviation for all features.
#' 
#' The total number of examples and the dimensionality of the data are chosen 
#' through the \code{num_examples} and \code{num_features} arguments. The 
#' \code{imbalance_ratio} argument together with \code{num_examples} 
#' determines the exact number of examples in the majority and minority 
#' classes. To simulate noise in the data, approximately \code{noise_min} 
#' examples in the majority class are labelled as minority class examples and 
#' approximately \code{noise_maj} examples in the minority class are labelled 
#' as majority class examples. \code{noise_maj} and \code{noise_min} are 
#' fractions.
#' 
#' @param num_examples Total number of examples in the data set.
#' @param num_features Total number of features in the data set.
#' @param imbalance_ratio Ratio of the number of examples in the majority 
#'  class to the number of examples in the minority class.
#' @param noise_maj Fraction of the minority class that is mislabelled as 
#'  majority class.
#' @param noise_min Fraction of the majority class that is mislabelled as 
#'  minority class.
#' @param seed Integer value for reproducibility purposes.
#' @return A data frame containing an imbalanced two-class data set.
#' @export

generate_imbalanced_data <- function(num_examples = 100L, num_features = 2L, 
                                     imbalance_ratio = 5, noise_maj = 0.05, 
                                     noise_min = 0.1, seed = NULL){
  if (!is.null(seed)) set.seed(seed)
  
  maj_size <- round(num_examples / (1 + 1 / imbalance_ratio))
  min_size <- num_examples - maj_size
  
  mu_maj <- rep(0, times = num_features)
  mu_min <- rep(2, times = num_features)
  
  Sigma_maj <- diag(1, nrow = num_features, ncol = num_features)
  Sigma_min <- diag(1, nrow = num_features, ncol = num_features)
  
  X_maj <- MASS::mvrnorm(maj_size, mu = mu_maj, Sigma = Sigma_maj)
  y_maj <- sample(0:1, size = maj_size, replace = TRUE, 
                  prob = c(1 - noise_min, noise_min))
  X_min <- MASS::mvrnorm(min_size, mu = mu_min, Sigma = Sigma_min)
  y_min <- sample(0:1, size = min_size, replace = TRUE, 
                  prob = c(noise_maj, 1 - noise_maj))
  
  X <- rbind(X_maj, X_min)
  y <- c(y_maj, y_min)
  
  imb_data <- cbind(X, y)
  imb_data <- as.data.frame(imb_data)
  colnames(imb_data) <- c(paste0("V", seq_len(num_features)), "target")
  imb_data[["target"]] <- factor(imb_data[["target"]], 
                                 levels = c(0, 1), 
                                 labels = c("Majority", "Minority"))
  
  shuffle_data(imb_data)
}

extract_classes <- function(y){
  structure(names(sort(table(y))), names = c("Minority", "Majority"))
}

shuffle_data <- function(data) data[sample(nrow(data)), , drop = FALSE]

compute_oversample_size <- function(majority_size, minority_size,
                                    perc_min, perc_over){
  if (!is.null(perc_over)){
    frac_over <- perc_over / 100
    return(round(frac_over * minority_size))
  } else{
    frac_min <- perc_min / 100
    data_size <- majority_size + minority_size
    sample_size <- (frac_min * data_size - minority_size) / (1 - frac_min)
    sample_size <- round(sample_size)
    if (sample_size < 0) sample_size <- 0
    return(sample_size)
  }
}

compute_undersample_size <- function(majority_size, minority_size,
                                     perc_maj, perc_under){
  if (!is.null(perc_under)){
    frac_under <- perc_under / 100
    frac_under_remove <- 1 - frac_under
    return(round(frac_under_remove * majority_size))
  } else{
    frac_maj <- perc_maj / 100
    data_size <- majority_size + minority_size
    sample_size <- (majority_size - frac_maj * data_size) / (1 - frac_maj)
    sample_size <- round(sample_size)
    if (sample_size < 0) sample_size <- 0
    return(sample_size)
  }
}

# compute_sample_size <- function(algorithm_type,
#                                 majority_size, minority_size,
#                                 perc_class, perc_change){
#   if (!is.null(perc_change)){
#     frac_change <- perc_change / 100
#     if (algorithm_type == "under-sampling"){
#       frac_under_remove <- 1 - frac_change
#       return(round(frac_under_remove * majority_size))
#     }
#     return(round(frac_change * minority_size))
#   } else{
#     frac_class <- perc_class / 100
#     data_size <- majority_size + minority_size
#     sample_size <- 
#       if (algorithm_type == "over-sampling"){
#         (frac_class * data_size - minority_size) / (1 - frac_class)
#       } else if (algorithm_type == "under-sampling"){
#         (majority_size - frac_class * data_size) / (1 - frac_class)
#       } else{
#         stop("Unknown algorithm type. Choose either ", 
#              "over-sampling or under-sampling.",
#              call. = TRUE)
#       }
#     sample_size <- round(sample_size)
#     if (sample_size < 0) sample_size <- 0
#     return(sample_size)
#   }
# }

knn <- function(data_train, data_test, k = 5, distance = 2, 
                kernel = "rectangular", remove_first_neighbour = FALSE){
  last_col <- ncol(data_train)
  colnames(data_train)[last_col] <- "Class"
  colnames(data_test)[last_col] <- "Class"
  
  result <- kknn::kknn(Class ~ .,
                       train = data_train,
                       test = data_test,
                       k = k + 1,
                       distance = distance,
                       kernel = kernel,
                       scale = TRUE)
  
  if (remove_first_neighbour){
    knn_indices <- 
      if (nrow(data_test) == 1) t(result$C)[, -1, drop = FALSE]
    else result$C[, -1, drop = FALSE]
    
    knn_classes <- result$CL[, -1, drop = FALSE]
  } else{
    knn_indices <- 
      if (nrow(data_test) == 1) t(result$C)[, 1:k, drop = FALSE]
    else result$C[, 1:k, drop = FALSE]
    
    knn_classes <- result$CL[, 1:k, drop = FALSE]
  }
  
  return(list(knn_indices = knn_indices, knn_classes = knn_classes))
}

synth_per_example <- function(sample_size, min_size){
  ratio <- sample_size / min_size
  upper_bound <- ceiling(ratio)
  lower_bound <- floor(ratio)
  upper_bound_prob <- 1 - (upper_bound - ratio)
  sample(c(lower_bound, upper_bound), size = min_size, 
         replace = TRUE, prob = c(1 - upper_bound_prob, upper_bound_prob))
}

center_and_scale <- function(X){
  nzv_features <- caret::nearZeroVar(X, saveMetrics = TRUE)$nzv
  X_nzv <- X[, nzv_features, drop = FALSE]
  X_not_nzv <- X[, !nzv_features, drop = FALSE]
  
  X_not_nzv <- scale(X_not_nzv, center = TRUE, scale = TRUE)
  center_not_nzv <- attr(X_not_nzv, "scaled:center")
  sc_not_nzv <- attr(X_not_nzv, "scaled:scale")
  
  center <- vector("numeric", length = ncol(X))
  center[nzv_features] <- 0
  center[!nzv_features] <- center_not_nzv
  
  sc <- vector("numeric", length = ncol(X))
  sc[nzv_features] <- 1
  sc[!nzv_features] <- sc_not_nzv
  
  X[, !nzv_features] <- X_not_nzv
  attr(X, "scaled:center") <- center
  attr(X, "scaled:scale") <- sc
  X
}

kmeans <- function(X, num_centers, max_iter = 100L, nstart = 10L){
  X <- center_and_scale(X)
  km <- stats::kmeans(X, 
                      centers = num_centers,
                      iter.max = max_iter,
                      nstart = nstart)
  
  centers <- km$centers
  centers <- sweep(centers, 2, attr(X, "scaled:scale"), "*")
  centers <- sweep(centers, 2, attr(X, "scaled:center"), "+")
  km$centers <- as.data.frame(centers)
  km
}

euclidian_distance <- function(A, x){
  sqrt(rowSums(sweep(A, 2, x, "-") ^ 2))
}