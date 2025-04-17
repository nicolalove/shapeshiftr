cvratio <- function(points, log10 = FALSE) {
  if (log10) {
    ind <- cvind(points, log10 = TRUE)
    pop <- cvpop(points, log10 = TRUE)
    ratio <- ind - pop
  } else {
    ind <- cvind(points)
    pop <- cvpop(points)
    ratio <- ind/pop
  }
  return(ratio)
}
