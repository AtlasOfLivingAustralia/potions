# potions
lightweight options management in R, designed to act like pkg 
[`here`](https://cran.r-project.org/package=here), but for nested lists in 
options. Currently experimental. For more comprehensive options
management try [`settings`](https://cran.r-project.org/package=settings).

`potions` contains three functions:
* `brew` puts lists into a slot in `options` with a user-specified name
* `pour` retrieves a value from that list, selected via one or more slot names
* `drain` empties the user-specified `options` slot

Example:
```
# set some example data
options_list <- list(
  data = list(x = 1, y = 2),
  metadata = list(a = 10, b = 12))
  
# extract values using `here()`-like syntax
pour("data", "x")
[1] 1
pour("data", "y")
[1] 2

# clean up
drain()

# prove cleanup worked:
getOption("potions_example")
NULL
```