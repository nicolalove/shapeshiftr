# Calculate Coefficient of Variation (CV) for Inter-individual Distances

This function calculates the coefficient of variation for
inter-individual distances at the population- and individual-level CV,
and the ratio between the two, within a data frame, optionally applying
log10 transformation.

## Usage

``` r
cvmetrics_sf(iid_dataset, distcol, idcol, grp_by, log10 = FALSE)
```

## Arguments

- iid_dataset:

  A data frame containing a column of inter-individual distances for a
  sampling unit.

- distcol:

  The name of the column containing the inter-individual distances in
  quotations, e.g., "iidist". This can be of class "numeric" or "units".

- idcol:

  The name of the column containing the IDs of the individuals in
  quotations, e.g., "animalID". If you used iidist() to create these
  values, then just choose one of the identification columns (either
  "ID1" or "ID2").

- grp_by:

  The variables to group by when calculating the CVs for each metric in
  quotations, e.g., c("yday", "year", "site").

- log10:

  A logical value indicating whether to apply a log10 transformation to
  the CV values.

## Value

A data frame with calculated CV values for the population, the
individual and the ratio between them for each grouping variable
specified (hour, day, season, etc).
