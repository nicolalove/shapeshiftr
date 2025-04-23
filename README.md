# shapeshiftr

## Overview

`shapeshiftr` calculates the inter-individual distances (IID) in a group
or population, and compares population- vs individual-level variation in
those distances to identify the spatial signatures of range shape and
change. `shapeshiftr` provides 3 functions to calculate the coefficient
of variation (CV) for simple location data:

-   `cvpop()` calculates the population-level CV of IID (CV of all
    pairwise distances)
-   `cvind()` calculates the individual-level CV of IID ( CV of the
    average pairwise distance per individual amoung all individuals)
-   `cvratio()` calculates the ratio of the individual-level CV to the
    population-level CV

And provides 2 functions for dataframes with an `sf` geometry column:

-   `iidist()` measures the inter-individual distance between every pair
    of individuals within a given sampling unit (day, season, patch)
-   `cvmetrics_sf()` measures the population- and individual-level CV of
    IID and their ratio within a given sampling unit (day, season,
    patch)

For a detailed introduction, please see `vignette("shapeshiftr")` or
[shapeshiftr](articles/shapeshiftr.html).

## Installation

For now, the package can be installed directly from GitHub one of two
ways.

*Recommended:*

``` r
install.packages("pak")
pak::pak("nicolalove/shapeshiftr")
```

*Or:*

``` r
install.packages("devtools")
devtools::install_github("nicolalove/shapeshiftr")
```

## Usage

`cvpop()`, `cvind()` and `cvratio()` produce a numeric value, or if used
within dplyr verbs, a column, where each row contains the CV of IID for
a given sampling unit (time, season, site). This is useful if you have
location data in a vector, matrix or dataframe without a geometry
column.

``` r
library(shapeshiftr)

 # a single numeric value
cvpop(split_population[, c("x", "y")]

# within a dataframe
split_population %>% group_by(year) %>% summarise(pop = cvpop(across(x,y)), ind = cvind(across(x,y)), ratio = cvratio(across(x,y)))
```

`iidist()` returns a dataframe containing a column of inter-individual
distances for a given sampling unit (time, season, site). This is useful
if you have a `sf` dataframe with locations stored in a geometry column.

```         
library(sf)
distances <- iid(asym_split_sf, nest_by = c("year"), idcol = "id")
head(distances)
```

`cvmetrics_sf` returns a dataframe with the three CV metrics in separate
columns. This is useful if you already have inter-individual distances
measured, or as downstream analysis after `iidist()`.

```         
cvdf <- cvmetrics_sf(distances, distcol = "iidist" idcol = "id", grp_by = c("year"))
head(cvdf)
```
