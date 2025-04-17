iidist <- function(dataset, nest_by = NULL, ...){
  if (is.null(nest_by) || length(nest_by) == 0) {
    stop("Please specify one or more columns to nest by, e.g., iidist(dataset, nest_by = c('year', 'season'))")
  }
  if (st_is_sf(dataset) == FALSE) {
    stop("Please convert your dataframe into an sf object using the function st_as_sf(dataframe, coords = c(X, Y), crs = #) from the sf package")
  }
  dataset %>%
    ungroup() %>%
    nest(data = -dplyr::all_of(nest_by)) %>%
    mutate(
      pairdist = map(data, ~{
        tryCatch({
          dist_matrix <- st_distance(.)
          n <- nrow(dist_matrix)
          if (is.null(n) || n == 0) {
            return(data.frame(ID1 = NA, ID2 = NA, iidist = NA))
          }
          combs <- expand.grid(ID1 = 1:n, ID2 = 1:n) %>%
            filter(ID1 < ID2) %>%
            mutate(iidist = dist_matrix[cbind(ID1, ID2)])
          # Add original IDs
          combs$ID1 <- rownames(.)[combs$ID1]
          combs$ID2 <- rownames(.)[combs$ID2]
          return(combs)
        }, error = function(e) {
          # Return NA in case of error
          return(data.frame(ID1 = NA, ID2 = NA, iidist = NA))
        })
      })
    ) %>%
    select(-data) %>%
    unnest(pairdist) %>%
    distinct()
}
