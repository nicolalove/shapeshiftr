#' Calculate Coefficient of Variation (CV) of Inter-individual Distances at the Population-level
#'
#' This function calculates the CV of the inter-individual distance between every pair of individuals, optionally applying a log10 transformation. We assume all individuals are sampled approximately within the same time point. If the input is a data frame of multiple sampling sessions, we suggest using this function in a pipe where the data frame is grouped, e.g., data %>% group_by(day, year) %>% summarise(cvpop = cvpop(c(x,y)))
#'
#' @param points A data frame, matrix, or vector of locations, where each row is a sampled individual.
#' @param log10 An optional argument for the log transformation of the CV of inter-individual distances.
#' @return A single numeric value for calculated coefficient of variation of the inter-individual distances between every pair of individuals. If this is used is a tidyverse pipe, the output will be a column of numbers.
#' @export
cvpop <- function(points, log10 = FALSE ) {
  dist_matrix <- as.matrix(dist(points))
  lower_tri_indices <- lower.tri(dist_matrix)
  lower_tri_distances <- dist_matrix[lower_tri_indices]
  if (log10) {
  CV_lower_tri_distances <- log10(sd(lower_tri_distances)/mean(lower_tri_distances))
  } else {
    CV_lower_tri_distances <- sd(lower_tri_distances)/mean(lower_tri_distances)
  }
  return(CV_lower_tri_distances)
}
