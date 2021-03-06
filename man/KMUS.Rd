% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/KMUS.R
\name{KMUS}
\alias{KMUS}
\title{The k-Means Under-Sampling algorithm.}
\usage{
KMUS(data, perc_maj = 50, perc_under = NULL, max_iter = 100L,
  nstart = 10L, classes = NULL)
}
\arguments{
\item{data}{A data frame containing the predictors and the outcome. The 
predictors must be numeric and the outcome must be both a binary valued 
factor and the last column of \code{data}.}

\item{perc_maj}{The desired \% size of the majority class relative to the
whole data set. For instance, if \code{perc_maj} = 50 a balanced version
of the input data set is returned. \code{perc_maj} is ignored if
\code{perc_under} is specified.}

\item{perc_under}{\% of examples to select from the majority class. If
specified \code{perc_maj} is ignored.}

\item{max_iter}{Maximum number of iterations of the k-Means algorithm.}

\item{nstart}{Number of random restarts of the k-Means algorithm.}

\item{classes}{A named vector identifying the majority and the minority
classes. The names must be "Majority" and "Minority". This argument is
only useful if the function is called inside another sampling function.}
}
\value{
A data frame containing a more balanced version of the input data 
 set after under-sampling it with the KMUS algorithm. As the majority 
 examples returned by KMUS are not examples of the input data set, original 
 order of the examples cannot be preserved. Thus, the returned data frame 
 contains all majority examples followed by all minority examples.
}
\description{
\code{KMUS} returns a more balanced version of a data set after 
under-sampling the majority class using the k-Means algorithm.
}
\details{
KMUS is an adaptation of the k-Means algorithm to work as an under-sampling 
algorithm. It clusters the majority class using the k-Means algorithm and 
uses the centroids computed as the representatives of the majority class.
}
