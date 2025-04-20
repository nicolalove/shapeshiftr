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
-   `cvmetrics_sf()` then measures the population- and individual-level
    CV of IID and their ratio within a given sampling unit (day, season,
    patch)

For a detailed introduction, please see `vignette("shapeshiftr")` or
[shapeshiftr](articles/shapeshiftr.html).

## Installation

For now, the package can be installed directly from GitHub one of two
ways. 

*Recommended: *
```r
install.packages("pak")
pak::pak("nicolalove/shapeshiftr")
```
*Or: *
```r
install.packages("devtools")
devtools::install_github("nicolalove/shapeshiftr")
```
## Background

The degree of social cohesion across a group depends on the distance
between individuals, because greater separation reduces their ability to
coordinate their actions. The spacing between individuals is thought to
reflect the balance between attractive forces (e.g., to avoid predation
and find mates) and repulsive forces (e.g., to avoid competition).
Changes in abundance or habitat can alter the balance of attractive and
repulsive forces governing group spacing patterns, leading to changes in
the spatial organization within the group/population, which could
manifest as changes in range shape (e.g., contraction, elongation or
splitting).

Determining the range and spatial density of individuals fully requires
extensive sampling. Here we add to the wide range of spatial analysis
tools by developing summary statistics that require less sampling to
describe key features of a population's range and detect changes over
time in spatial use. Specifically, we use two different measures of the
coefficient of variation (CV) in IID: individual-level and
population-level. In either case, the coefficient of variation provides
a scale-free measure that remains the same if density declines without a
change in spatial distribution or if the range contracts symmetrically
as a population declines in abundance. Changes in range shape and/or
splitting into subpopulations will, however, change these two CV
measures in distinct ways.

### Reference

For the full description of the method and its application, see: Love &
Otto (2025). *Spatial Signatures of Population Cohesion*. Methods in
Ecology and Evolution. <https://doi.org/>
