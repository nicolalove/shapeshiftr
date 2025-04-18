#' Calculate Coefficient of Variation (CV) for Inter-individual Distances
#'
#' This function calculates the coefficient of variation for inter-individual
#' distances at the population- and individual-level CV, and the ratio between the two, within a data frame, optionally applying log10 transformation.
#'
#' @param iid_dataset A data frame containing a column of inter-individual
#'   distances for a given time unit.
#' @param iidist_col The name of the column containing the inter-individual
#'   distances.
#' @param id_col The name of the column containing the IDs of the individuals. If you used iidist() to create these values, then just choose one of the identification columns (either ID1 or ID2).
#' @param log10 A logical value indicating whether to apply a log10 transformation
#'   to the CV values.
#' @param ... The variables to group by when calculating the CVs for each metric (within the same hour, day, patch, season, etc).
#'
#' @return A data frame with calculated CV values for the population, the individual and
#'   and the ratio between them for each grouping variable specified (hour, day, season, etc).
#' @export
#'
#' @import dplyr
#' @import sf
#' @import rlang
#' @importFrom magrittr %>%
#' @import tidyr
#' @importFrom purrr map


cvmetrics <- function(iid_dataset,  iidist_col, id_col,...,log10 = FALSE){
  grp_syms <- rlang::enquos(...)
  id_q     <- rlang::enquo(id_col)
  iidist_q <- rlang::enquo(iidist_col)
  if (length(grp_syms) == 0) {
    stop("Please specify one or more grouping columns, e.g. cvmetrics(df, distance, animalID, yday, hour)")
  }
  if (length(iidist_q) == 0) {
    stop("Please specify the column that contains the inter-individual distances by whatever time-unit you specified in the `grp_by` argument. If you used iidist() to calculate this, the column name will be `iidist`. e.g.,cvmetrics(dataset, iidist, animalID, yday, hour)")
  }
  if (length(id_q) == 0) {
    stop("Please specify the column that contains the names/IDs of the individuals, e.g., cvmetrics(dataset, iidist, animalID, yday, hour)")
  }
  iid_dataset <- iid_dataset %>%
    mutate(.val = as.numeric(!!iidist_q))
  if (log10) {
    popcv <- iid_dataset %>% group_by(!!!grp_syms) %>%
        summarise(cvpop = log10(sd(.val, na.rm = T)/mean(.val, na.rm = T)))
    indivcv <- iid_dataset %>% group_by(!!id_q, !!!grp_syms) %>%
      summarise(meanii = mean(.val, na.rm = T)) %>% ungroup() %>%
      group_by(!!!grp_syms) %>%
      summarise(cvind = if_else(mean(meanii, na.rm = T) == 0, 0,
                                log10(sd(meanii,na.rm = T)/mean(meanii, na.rm = T))))
    total <- left_join(popcv, indivcv)
    total$ratio <- total$cvind - total$cvpop
  } else {
    popcv <- iid_dataset %>% group_by(!!!grp_syms) %>%
        summarise(cvpop = sd(.val, na.rm = T)/mean(.val, na.rm = T))
    indivcv <- iid_dataset %>% group_by(!!id_q,!!!grp_syms) %>%
      summarise(meanii = mean(.val, na.rm = T)) %>% ungroup() %>%
      group_by(!!!grp_syms) %>%
      summarise(cvind = if_else(mean(meanii, na.rm = T) == 0, 0,
                                sd(meanii,na.rm = T)/mean(meanii, na.rm = T)))
    total <- left_join(popcv, indivcv)
    total$ratio <- total$cvind / total$cvpop
  }
  return(total)

}
