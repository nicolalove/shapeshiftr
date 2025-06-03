#' Calculate the Inter-individual Distance Between Every Pair of Individuals
#'
#' This function calculates the inter-individual distance between every pair of individuals within a sampling unit in an sf data frame.
#'
#' @param dataset An sf data frame containing a geometry column
#' @param nest_by A string of column names for the time unit, patch, or other covariate to the calculate the distance over - this should be the smallest time unit possible so there aren't multiple instances of the same individual in the period. For example, for a daily sampling frequency where the user is interested in comparing seasons, these columns would be "yday" and "season." To compare within each patch (of different populations), then this could be yday, season, patch.
#' @param idcol The name of the column containing the IDs of the individuals in quotations, e.g., "animalID".
#' @return A data frame containing a column of inter-individual distances over the specified time periods.
#' @export
#'
#' @import dplyr
#' @import sf
#' @import rlang
#' @importFrom magrittr %>%
#' @import tidyr
#' @importFrom purrr map
#' @importFrom stats dist sd
#' @importFrom utils data

iidist <- function(dataset, nest_by, idcol){
  if (is.null(nest_by) || length(nest_by) == 0) {
    stop("Please specify one or more columns to nest by, e.g., iidist(dataset, nest_by = c('year', 'season'))")
  }
  if (is.null(idcol) || length(idcol) == 0) {
    stop("Please specify the column that contains the names/IDs of the individuals")
  }
  if (inherits(dataset, "sf") == "FALSE") {
    stop("Please convert your dataframe into an sf object using the function st_as_sf(dataframe, coords = c(X, Y), crs = #) from the sf package")
  }
  idcol_q <- rlang::enquo(idcol)
  dataset %>%
    ungroup() %>%
    nest(data = -all_of(nest_by)) %>%
    mutate(
      pairdist = purrr::map(data, ~{
        df <- .x
        if (nrow(df) < 2) {
          return(tibble(ID1 = NA, ID2 = NA, iidist = NA))
        }
        tryCatch({
          dist_matrix <- st_distance(df)
          ids <- pull(df, !!idcol_q)
          combs <- expand.grid(ID1 = ids, ID2 = ids) %>% filter(ID1 != ID2)

          idx1 <- match(combs$ID1, ids)
          idx2 <- match(combs$ID2, ids)

          combs$iidist <- dist_matrix[cbind(idx1, idx2)]
          return(combs)
        }, error = function(e) {
          return(tibble(ID1 = NA, ID2 = NA, iidist = NA))
        })
      })
    ) %>%
    dplyr::select(-data) %>%
    unnest(pairdist) %>%
    distinct()
}
