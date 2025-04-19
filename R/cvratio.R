#' Calculate the Ratio of Individual- to Population-level Coefficient of Variation (CV) of Inter-individual Distances
#'
#' This function calculates the ratio of individual-level CVs to population-level CVs of inter-individual distances, optionally applying a log10 transformation. We assume all individuals are sampled approximately within the same time point. If the input is a data frame with multiple sampling sessions, we suggest using this function in a pipe where the data frame is grouped, e.g., data %>% group_by(day, year) %>% summarise(cvratio = cvratio(across(c(x,y)))). **Note the use of across()!**
#'
#' @param points A data frame, matrix, or two vectors of locations, where each row is a sampled individual.
#' Syntax examples for different input types:
#' \itemize{
#'   \item If `points` is a data frame: specify the columns, e.g., `cvratio(df[, c("x", "y")])`
#'   \item If `points` is a matrix: input directly, e.g., `cvratio(m)`
#'   \item If `points` are two vectors: bind them, e.g., `cvratio(cbind(x, y))`
#' }
#' @param log10 An optional argument for the log transformation of the ratio of CVs.
#' @export
cvratio <- function(points, log10 = FALSE) {
  if (is.atomic(points) && is.vector(points) && is.null(dim(points))) {
    stop("It looks like you're passing a single vector. If you're using two location vectors, please use `cbind(x, y)` instead of `c(x, y)`, as `c()` flattens the inputs.")
  }
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
