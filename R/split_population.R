#' Data to showcase a population fragmenting into two subpopulations
#'
#' A dataset containing locations of sampled animals over 2 years
#'
#' @format A data frame with 100 rows and 5 columns.
#' describe{
#' item{x}{Longitude of animal}
#' item{y}{Latitude of animal}
#' item{year}{Year animal was sampled}
#' item{random}{A column containing data but not relevant to these functions}
#' item{id}{The name of the animal sampled}
#' }
#' @source Created by Nicola Love, by creating 3 bivariate normal distributions using `MASS::mvrnorm()`. All three distributions had a standard deviation of 15, the distribution in year 1 was centered at c(50,50), the two distributions in year 2 were centered at c(50,0) and c(50, 100).
#'
#' @examples
#' data(split_population)
"split_population"
