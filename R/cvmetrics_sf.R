cvmetrics <- function(iid_dataset, grp_by = NULL, iidist_column = NULL, log10 = FALSE){
    if (is.null(grp_by) || length(grp_by) == 0) {
      stop("Please specify one or more columns to group the inter-individual distances by, e.g., cvmetrics(dataset, grp_by = c('hour', 'yday'))")
    }
  if (is.null(iidist_column) || length(iidist_column) == 0) {
    stop("Please specify the column that contains the inter-individual distances by whatever time-unit you specified in the `grp_by` argument. If you used iidist() to calculate this, the column name will be `iidist`. e.g.,cvmetrics(dataset, grp_by = c('hour', 'yday'), iidist_column = iidist)")
  }
  if (log10) {
    popcv <- iid_dataset %>% group_by(all_of(grp_by)) %>% summarise(cvpop = log10(sd({{iidist_column}}, na.rm = T)/mean({{iidist_column}}, na.rm = T)))
  } else {
    popcv <- iid_dataset %>% group_by(all_of(grp_by)) %>% summarise(cvpop = sd({{iidist_column}}, na.rm = T)/mean({{iidist_column}}, na.rm = T))
  }


}
