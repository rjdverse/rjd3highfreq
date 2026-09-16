# Extract an ARIMA model from a Java object

This internal function retrieves the ARIMA specification associated with
a given path in a Java object. It extracts the model structure
(description string, AR/MA polynomials, differencing order) and the
innovation variance, and returns them as an R \`arima_model\` object.

## Usage

``` r
.arima_extract(jrslt, path)
```

## Arguments

- jrslt:

  Java object containing the estimated RegARIMA.

- path:

  Character string specifying the extraction path within the Java object

## Value

An \`arima_model\` object as constructed by
\`rjd3toolkit::arima_model()\`.
