iidist <- function(dataset, nest_by = NULL, idcol = NULL, ...){
  if (is.null(nest_by) || length(nest_by) == 0) {
    stop("Please specify one or more columns to nest by, e.g., iidist(dataset, nest_by = c('year', 'season'))")
  }
  if (is.null(idcol) || length(idcol) == 0) {
    stop("Please specify the column that contains the names/IDs of the individuals")
  }
  if (class(dataset) != "sf") {
    stop("Please convert your dataframe into an sf object using the function st_as_sf(dataframe, coords = c(X, Y), crs = #) from the sf package")
  }
  idcol_q <- rlang::enquo(idcol)
  dataset %>%
    ungroup() %>%
    nest(data = -dplyr::all_of(nest_by)) %>%
    mutate(
      pairdist = map(data, ~{
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
