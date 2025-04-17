#' @import dplyr
#' @import sf
#' @importFrom magrittr %>%
#' @import tidyr
#' @import purrr

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
