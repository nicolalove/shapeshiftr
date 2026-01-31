# Calculate the Ratio of Individual- to Population-level Coefficient of Variation (CV) of Inter-individual Distances

This function calculates the ratio of individual-level CVs to
population-level CVs of inter-individual distances, optionally applying
a log10 transformation. We assume all individuals are sampled
approximately within the same time point. If the input is a data frame
with multiple sampling sessions, we suggest using this function in a
pipe where the data frame is grouped, e.g., data %\>% group_by(day,
year) %\>% summarise(cvratio = cvratio(across(c(x,y)))). **Note the use
of across()!**

## Usage

``` r
cvratio(points, log10 = FALSE)
```

## Arguments

- points:

  A data frame, matrix, or two vectors of locations, where each row is a
  sampled individual. Syntax examples for different input types:

  - If `points` is a data frame: specify the columns, e.g.,
    `cvratio(df[, c("x", "y")])`

  - If `points` is a matrix: input directly, e.g., `cvratio(m)`

  - If `points` are two vectors: bind them, e.g., `cvratio(cbind(x, y))`

- log10:

  An optional argument for the log transformation of the ratio of CVs.
