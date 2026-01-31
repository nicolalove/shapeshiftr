# Calculate Coefficient of Variation (CV) of Inter-individual Distances at the Individual-level

This function calculates the CV of average inter-individual distance
amoung individuals, optionally applying a log10 transformation. We
assume all individuals are sampled approximately within the same time
point. If the input is a data frame with multiple sampling sessions, we
suggest using this function in a pipe where the data frame is grouped,
e.g., data %\>% group_by(day, year) %\>% summarise(cvind =
cvind(across(c(x,y)))). **Note the use of across()!**

## Usage

``` r
cvind(points, log10 = FALSE)
```

## Arguments

- points:

  A data frame, matrix, or two vectors of locations, where each row is a
  sampled individual. Syntax examples for different input types:

  - If `points` is a data frame: specify the columns, e.g.,
    `cvind(df[, c("x", "y")])`

  - If `points` is a matrix: input directly, e.g., `cvind(m)`

  - If `points` are two vectors: bind them, e.g., `cvind(cbind(x, y))`

- log10:

  An optional argument for the log transformation of the CV of
  inter-individual distances.

## Value

A single numeric value for calculated coefficient of variation of the
average inter-individual distance amoung individuals. If this is used is
a tidyverse pipe, the output will be a column of numbers.
