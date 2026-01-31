# Data to showcase a population fragmenting into two subpopulations

A dataset containing locations of sampled animals over 2 years

## Usage

``` r
split_population
```

## Format

A data frame with 100 rows and 5 columns.

- x:

  Longitude of animal

- y:

  Latitude of animal

- year:

  Year animal was sampled

- random:

  A column containing data but not relevant to these functions

- id:

  The name of the animal sampled

## Source

Created by Nicola Love, by creating 3 bivariate normal distributions
using [`MASS::mvrnorm()`](https://rdrr.io/pkg/MASS/man/mvrnorm.html).
All three distributions had a standard deviation of 15, the distribution
in year 1 was centered at c(50,50), the two distributions in year 2 were
centered at c(50,0) and c(50, 100).

## Examples

``` r
data(split_population)
```
