cvmetrics <- function(iid_dataset, grp_by = NULL, iidist_col = NULL, idcol = NULL, log10 = FALSE){
    if (is.null(grp_by) || length(grp_by) == 0) {
      stop("Please specify one or more columns to group the inter-individual distances by, e.g., cvmetrics(dataset, grp_by = c('hour', 'yday'))")
    }
  if (is.null(iidist_col) || length(iidist_col) == 0) {
    stop("Please specify the column that contains the inter-individual distances by whatever time-unit you specified in the `grp_by` argument. If you used iidist() to calculate this, the column name will be `iidist`. e.g.,cvmetrics(dataset, grp_by = c('hour', 'yday'), iidist_col = iidist)")
  }
  if (is.null(idcol) || length(idcol) == 0) {
    stop("Please specify the column that contains the names/IDs of the individuals, e.g., cvmetrics(dataset, grp_by = c('hour', 'yday'), iidist_col = AID)")
  }
  if (log10) {
    popcv <- iid_dataset %>% group_by(all_of(grp_by)) %>%
        summarise(cvpop = log10(sd({{iidist_col}}, na.rm = T)/mean({{iidist_col}}, na.rm = T)))
    indivcv <- iid_dataset %>% group_by(idcol, all_of(grp_by)) %>%
      summarise(meanii = mean({{iidist_col}}, na.rm = T)) %>% ungroup() %>%
      group_by(all_of(grp_by)) %>%
      summarise(cvind = if_else(mean(meanii, na.rm = T) == 0, 0,
                                log10(sd(meanii,na.rm = T)/mean(meanii, na.rm = T))))
    total <- left_join(popcv, indivcv)
    total$ratio <- total$indivcv - total$popcv
  } else {
    popcv <- iid_dataset %>% group_by(all_of(grp_by)) %>%
        summarise(cvpop = sd({{iidist_col}}, na.rm = T)/mean({{iidist_col}}, na.rm = T))
    indivcv <- iid_dataset %>% group_by(idcol, all_of(grp_by)) %>%
      summarise(meanii = mean({{iidist_col}}, na.rm = T)) %>% ungroup() %>%
      group_by(all_of(grp_by)) %>%
      summarise(cvind = if_else(mean(meanii, na.rm = T) == 0, 0,
                                sd(meanii,na.rm = T)/mean(meanii, na.rm = T)))
    total <- left_join(popcv, indivcv)
    total$ratio <- total$indivcv / total$popcv
  }


}
