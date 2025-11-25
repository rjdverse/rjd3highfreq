# Plot Function for JDFractionalAirlineDecomposition Objects

This function creates a plot for the result of an Arima Model Based
(AMB) decomposition of one or several frequencies (class
\`JDFractionalAirlineDecomposition\`). It shows the decomposition and
the component of the model.

## Usage

``` r
# S3 method for class 'JDFractionalAirlineDecomposition'
plot(x, from, to, type_chart = c("y-sa-trend", "cal-seas-irr"), ...)
```

## Arguments

- x:

  An object of class 'JDFractionalAirlineDecomposition'.

- from:

  \`Date\` or \`POSIXt\` object, optional starting point for x-axis.

- to:

  \`Date\` or \`POSIXt\` object, optional ending point for x-axis.

- type_chart:

  Character vector specifying the type of chart to plot ("y-sa-trend",
  "cal-seas-irr").

- ...:

  Additional graphical parameters.

## Value

\`NULL\` (invisible).
