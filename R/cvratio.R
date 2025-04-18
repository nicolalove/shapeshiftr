#' Calculate the Ratio of Individual- to Population-level Coefficient of Variation (CV) of Inter-individual Distances
#'
#' This function calculates the ratio of individual-level CVs to population-level CVs of inter-individual distances, optionally applying a log10 transformation. We assume all individuals are sampled approximately within the same time point. If the input is a data frame of multiple sampling sessions, we suggest using this function in a pipe where the data frame is grouped, e.g., data %>% group_by(day, year) %>% summarise(cvratio = cvratio(c(x,y)))
#'
#' @param points A data frame, matrix, or vector of locations, where each row is a sampled individual.
#' @param log10 An optional argument for the log transformation of the ratio of CVs.
#' @export
cvratio <- function(points, log10 = FALSE) {
  if (log10) {
    ind <- cvind(points, log10 = TRUE)
    pop <- cvpop(points, log10 = TRUE)
    ratio <- ind - pop
  } else {
    ind <- cvind(points)
    pop <- cvpop(points)
    ratio <- ind/pop
  }
  return(ratio)
}
