#' Calculate Coefficient of Variation (CV) for Inter-individual Distances
#'
#' This function calculates the coefficient of variation for inter-individual
#' distances at the population- and individual-level CV, and the ratio between the two, within a data frame, optionally applying log10 transformation.
#'
#' @param iid_dataset A data frame containing a column of inter-individual
#'   distances for a sampling unit.
#' @param distcol The name of the column containing the inter-individual
#'   distances in quotations, e.g., "iidist". This can be of class "numeric" or "units".
#' @param idcol The name of the column containing the IDs of the individuals in quotations, e.g., "animalID". If you used iidist() to create these values, then just choose one of the identification columns (either "ID1" or "ID2").
#' @param log10 A logical value indicating whether to apply a log10 transformation
#'   to the CV values.
#' @param grp_by The variables to group by when calculating the CVs for each metric in quotations, e.g., c("yday", "year", "site").
#'
#' @return A data frame with calculated CV values for the population, the individual and the ratio between them for each grouping variable specified (hour, day, season, etc).
#' @export
#'
#' @import dplyr
#' @import sf
#' @import rlang
#' @importFrom magrittr %>%
#' @import tidyr
#' @importFrom purrr map


cvmetrics_sf <- function(iid_dataset,  distcol, idcol, grp_by, log10 = FALSE){
  # Convert string column names to symbols
  iidist_sym <- rlang::sym(distcol)
  id_sym     <- rlang::sym(idcol)
  grp_syms <- rlang::syms(grp_by)
  if (length(grp_syms) == 0) {
    stop("Please specify one or more grouping columns (in quotes), e.g. cvmetrics(df, ...,`yday`, `hour`)")
  }
  if (length(iidist_sym) == 0) {
    stop("Please spec
         ify the column (in quotes) that contains the inter-individual distances by whatever time-unit you specified in the `grp_by` argument. If you used iidist() to calculate this, the column name will be `iidist`. e.g.,cvmetrics(df,`iidist`...)")
  }
  if (length(id_sym) == 0) {
    stop("Please specify the column (in quotes) that contains the names/IDs of the individuals, e.g., cvmetrics(df, ...,`animalID`, ...)")
  }
  iid_dataset <- iid_dataset %>%
    mutate(.val = as.numeric(!!iidist_sym))
  if (log10) {
    popcv <- iid_dataset %>% group_by(!!!grp_syms) %>%
        summarise(cvpop = log10(sd(.val, na.rm = T)/mean(.val, na.rm = T)))
    indivcv <- iid_dataset %>% group_by(!!id_sym, !!!grp_syms) %>%
      summarise(meanii = mean(.val, na.rm = T)) %>% ungroup() %>%
      group_by(!!!grp_syms) %>%
      summarise(cvind = if_else(mean(meanii, na.rm = T) == 0, 0,
                                log10(sd(meanii,na.rm = T)/mean(meanii, na.rm = T))))
    total <- left_join(popcv, indivcv)
    total$ratio <- total$cvind - total$cvpop
  } else {
    popcv <- iid_dataset %>% group_by(!!!grp_syms) %>%
        summarise(cvpop = sd(.val, na.rm = T)/mean(.val, na.rm = T))
    indivcv <- iid_dataset %>% group_by(!!id_sym,!!!grp_syms) %>%
      summarise(meanii = mean(.val, na.rm = T)) %>% ungroup() %>%
      group_by(!!!grp_syms) %>%
      summarise(cvind = if_else(mean(meanii, na.rm = T) == 0, 0,
                                sd(meanii,na.rm = T)/mean(meanii, na.rm = T)))
    total <- left_join(popcv, indivcv)
    total$ratio <-total$cvind / total$cvpop
  }
  return(total)

}
