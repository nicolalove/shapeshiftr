cvpop <- function(points, log10 = FALSE ) {
  dist_matrix <- as.matrix(dist(points))
  lower_tri_indices <- lower.tri(dist_matrix)
  lower_tri_distances <- dist_matrix[lower_tri_indices]
  if (log10) {
  CV_lower_tri_distances <- log10(sd(lower_tri_distances)/mean(lower_tri_distances))
  } else {
    CV_lower_tri_distances <- sd(lower_tri_distances)/mean(lower_tri_distances)
  }
  return(CV_lower_tri_distances)
}
