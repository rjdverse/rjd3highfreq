# Extract a specific UCM component from a Java UcarimaModel object

This internal function retrieves a single component from a UCM
(Unobserved Components Model) result object returned by the Java
backend. The component can be specified either by name or by its integer
code.

## Usage

``` r
.ucm_extract(jrslt, cmp)
```

## Arguments

- jrslt:

  Java UcarimaModel object.

- cmp:

  Character string or integer specifying the component to extract. Valid
  components are:

  - "Series" or 1 : Complete series (Trend + Seasonal + Irregular +
    CalendarEffect)

  - "Trend" or 2 : Trend / level component

  - "Seasonal" or 3 : Seasonal component

  - "SeasonallyAdjusted" or 4 : Trend + Seasonal + CalendarEffect

  - "Irregular" or 5 : Irregular / residual component

  - "CalendarEffect" or 6 : Calendar effects (e.g., holidays)

## Value

The requested component extracted from the UCM result.

## Examples

``` r
if (FALSE) { # \dontrun{
# Assume `jucm` is a UcarimaModel Java object

# Extract the trend component
trend <- .ucm_extract(jucm, "Trend")
} # }
```
