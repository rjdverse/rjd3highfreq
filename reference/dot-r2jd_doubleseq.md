# Convert an R numeric vector to a Java DoubleSeq object

This internal helper function converts an R object to a Java `DoubleSeq`
object from the JDemetra+ toolkit.

## Usage

``` r
.r2jd_doubleseq(x)
```

## Arguments

- x:

  An object that can be coerced to a numeric vector.

## Value

A Java object of class `jdplus.toolkit.base.api.data.DoubleSeq`.

## Details

The input is first coerced to numeric using
[`as.numeric()`](https://rdrr.io/r/base/numeric.html) and is then passed
to the static Java factory method `DoubleSeq.of()`.

This function is mainly intended for internal use when passing numeric R
vectors to Java methods that expect a `DoubleSeq`.

## Examples

``` r
if (FALSE) { # \dontrun{
x <- c(1.2, 3.4, 5.6)
jseq <- .r2jd_doubleseq(x)
} # }
```
