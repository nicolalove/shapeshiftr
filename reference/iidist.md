# Calculate the Inter-individual Distance Between Every Pair of Individuals

This function calculates the inter-individual distance between every
pair of individuals within a sampling unit in an sf data frame.

## Usage

``` r
iidist(dataset, nest_by, idcol)
```

## Arguments

- dataset:

  An sf data frame containing a geometry column

- nest_by:

  A string of column names for the time unit, patch, or other covariate
  to the calculate the distance over - this should be the smallest time
  unit possible so there aren't multiple instances of the same
  individual in the period. For example, for a daily sampling frequency
  where the user is interested in comparing seasons, these columns would
  be "yday" and "season." To compare within each patch (of different
  populations), then this could be yday, season, patch.

- idcol:

  The name of the column containing the IDs of the individuals in
  quotations, e.g., "animalID".

## Value

A data frame containing a column of inter-individual distances over the
specified time periods.
