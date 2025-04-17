cvind <- function(points, log10 = FALSE) {
  dist_matrix <- as.matrix(dist(points))
  individual_distances <- vector("list", nrow(points))
  names(individual_distances) <- rownames(points)
  for (i in 2:nrow(dist_matrix)) {
    for (j in 1:(i-1)) {
      individual_distances[[i]] <- c(individual_distances[[i]], dist_matrix[i, j])
      individual_distances[[j]] <- c(individual_distances[[j]], dist_matrix[i, j])
    }
  }
  mean_values <- numeric(nrow(points))
  for (i in 1:nrow(points)) {
    pairwise_distances <- individual_distances[[i]]
    mean_dist <- mean(pairwise_distances)
    if (mean_dist != 0) {
      mean_values[i] <- mean_dist
    } else {
      mean_values[i] <- NA  # Handle cases where the mean distance is zero
    }
  }
  CV_mean_values <- sd(mean_values, na.rm = T)/mean(mean_values, na.rm = T)
  if (log10) {
    CV_mean_values <- log10(sd(mean_values)/mean(mean_values))
  }
  return(CV_mean_values)
}
