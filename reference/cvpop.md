# Calculate Coefficient of Variation (CV) of Inter-individual Distances at the Population-level

This function calculates the CV of the inter-individual distance between
every pair of individuals, optionally applying a log10 transformation.
We assume all individuals are sampled approximately within the same time
point. If the input is a data frame with multiple sampling sessions, we
suggest using this function in a pipe where the data frame is grouped,
e.g., data %\>% group_by(day, year) %\>% summarise(cvpop =
cvpop(across(c(x,y)))). **Note the use of across()!**

## Usage

``` r
cvpop(points, log10 = FALSE)
```

## Arguments

- points:

  A data frame, matrix, or two vectors of locations, where each row is a
  sampled individual. Syntax examples for different input types:

  - If `points` is a data frame: specify the columns, e.g.,
    `cvpop(df[, c("x", "y")])`

  - If `points` is a matrix: input directly, e.g., `cvpop(m)`

  - If `points` are two vectors: bind them, e.g., `cvpop(cbind(x, y))`

- log10:

  An optional argument for the log transformation of the CV of
  inter-individual distances.

## Value

A single numeric value for calculated coefficient of variation of the
inter-individual distances between every pair of individuals. If this is
used is a tidyverse pipe, the output will be a column of numbers.
