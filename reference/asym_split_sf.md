# Spatial dataframe to showcase asymmetrical fragmentation

An sf dataframe containing a geometry column column with locations of
sampled animals over 2 years.

## Usage

``` r
asym_split_sf
```

## Format

A dataframe with 100 rows and 4 columns.

- year:

  A column containing the year animals were sampled

- random:

  A column containing data but not relevant to these functions

- id:

  The name of the animal sampled

- geometry:

  An `sf` geometry column storing spatial point coordinates for each
  sampled individual.

## Source

Created by Nicola Love, by creating 3 bivariate normal distributions
generated with
[`MASS::mvrnorm()`](https://rdrr.io/pkg/MASS/man/mvrnorm.html), then
converted to an `sf` dataframe with unprojected geographic coordinates
(EPSG:4326). All three distributions had a standard deviation of
\\sqrt{.001}\\. In year 1, the distribution was centered at
c(29.285,-22.373) in the Venetia-Limpopo Nature Reserve. The two
distributions in year 2 were centered at the northern and southern ends
of the reserve: c(29.285,-22.15) and c(29.285,-22.45), respectively.

## Examples

``` r
data(asym_split_sf)
```
