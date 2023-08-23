
<!-- README.md is generated from README.Rmd. Please edit that file -->
<img src="man/figures/logo.png" align="left" style="margin: 0px 25px 0px 0px;" alt="" width="160"/>
<h2>
potions
</h2>
<h4>
<em>easy options management</em>
</h4>

<br> <br> <br> <br> <br>

<h3>
Overview
</h3>

Often it is useful to set bespoke options for a single workflow, or
within a single package, without altering global options that influence
other users or packages. This is possible using `base::options()` and
related functions, however doing so requires care, and occasionally some
bespoke knowledge. `potions` makes options management as easy as
possible, by decreasing programmers’ cognitive burden while storing and
retrieving information. It does this by following three guiding
principles:

- **minimalist**: `potions` has only three core functions: `brew()`,
  `pour()` and `drain()`
- **laconic**: functions use as few characters as possible
- **familiar**: uses a UI for data retrieval based on the `here` package

In combination, these features should make it easy for users and
developers to manage options using `potions`.

To install from CRAN:

``` r
install.packages("potions")
```

To install from GitHub:

``` r
install.packages("remotes")
remotes::install_github("atlasoflivingaustralia/potions")
```

To store data in `options()`, use `brew()`

``` r
library(potions)
brew(list(x = 1, y = list(a = 2, b = 4))) # use a list

brew(x = 1) # or as named arguments
```

Then you can use `pour()` to get the information you need:

``` r
pour() |> str() # get all data
#> List of 2
#>  $ x: num 1
#>  $ y:List of 2
#>   ..$ a: num 2
#>   ..$ b: num 4

pour("x") # get a subset of data
#> [1] 1

pour("y", "a") # for nested data
#> [1] 2
```

When you are done, simply use `drain()` to clean up:

``` r
drain()

pour() # nothing to return
#> list()
```
