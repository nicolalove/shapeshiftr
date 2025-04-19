#' Calculate Coefficient of Variation (CV) of Inter-individual Distances at the Individual-level
#'
#' This function calculates the CV of average inter-individual distance amoung individuals, optionally applying a log10 transformation. We assume all individuals are sampled approximately within the same time point. If the input is a data frame with multiple sampling sessions, we suggest using this function in a pipe where the data frame is grouped, e.g., data %>% group_by(day, year) %>% summarise(cvind = cvind(across(c(x,y)))). **Note the use of across()!**
#'
#' @param points A data frame, matrix, or two vectors of locations, where each row is a sampled individual.
#' Syntax examples for different input types:
#' \itemize{
#'   \item If `points` is a data frame: specify the columns, e.g., `cvind(df[, c("x", "y")])`
#'   \item If `points` is a matrix: input directly, e.g., `cvind(m)`
#'   \item If `points` are two vectors: bind them, e.g., `cvind(cbind(x, y))`
#' }
#' @param log10 An optional argument for the log transformation of the CV of inter-individual distances.
#' @return A single numeric value for calculated coefficient of variation of the average inter-individual distance amoung individuals. If this is used is a tidyverse pipe, the output will be a column of numbers.
#' @export
cvind <- function(points, log10 = FALSE) {
  if (is.atomic(points) && is.vector(points) && is.null(dim(points))) {
    stop("It looks like you're passing a single vector. If you're using two location vectors, please use `cbind(x, y)` instead of `c(x, y)`, as `c()` flattens the inputs.")
  }
  dist_matrix <- as.matrix(dist(points))
  individual_distances <- vector("list", nrow(points))
    names(individual_distances) <- rownames(points)
  for (i in 2:nrow(dist_matrix)) {
    for (j in 1:(i-1)) {
      individual_distances[[i]] <- c(individual_distances[[i]], dist_matrix[i, j])
      individual_distances[[j]] <- c(individual_distances[[j]], dist_matrix[i, j])
    }
  }
  mean_values <- numeric(nrow(points))
  for (i in 1:nrow(points)) {
    pairwise_distances <- individual_distances[[i]]
    mean_dist <- mean(pairwise_distances)
    if (mean_dist != 0) {
      mean_values[i] <- mean_dist
    } else {
      mean_values[i] <- NA  # Handle cases where the mean distance is zero
    }
  }
  CV_mean_values <- sd(mean_values, na.rm = T)/mean(mean_values, na.rm = T)
  if (log10) {
    CV_mean_values <- log10(sd(mean_values)/mean(mean_values))
  }
  return(CV_mean_values)
}
