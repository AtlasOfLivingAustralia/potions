# Internal code to parse `...` info passed to `brew()`
# Note that the approach used below is taken from advanced R:
# https://adv-r.hadley.nz/expressions.html

#' parse_quosures
#' @noRd
#' @importFrom rlang abort
#' @importFrom rlang quo_is_symbol
#' @keywords internal
parse_quosures <- function(dots){
  name_length <- any(length(names(dots) > 0)) & any(names(dots) != "")
  if(name_length){
    bullets <- c(
      "We detected a named input.",
      i = "This usually means that you've used `=` instead of `==`.")
    abort(bullets)
  }
  if(length(dots) > 0){
    do.call(c, lapply(dots, switch_expr_type))
  }else{
    NULL
  }
}

#' Switch functions for quosures
#' @param x A (single) quosure
#' @importFrom rlang abort
#' @importFrom rlang quo_get_expr
#' @importFrom rlang quo_get_env
#' @noRd
#' @keywords internal
switch_expr_type <- function(x){
  switch(expr_type(x),
         "symbol" = {parse_symbol(x)},
         "call" = {parse_call(x)},
         "literal" = {quo_get_expr(x)},
         abort("Quosure type not recognised")
  )
}

#' Get type from quosures
#' @param x A (single) quosure
#' @importFrom rlang quo_is_symbol
#' @importFrom rlang quo_is_call
#' @importFrom rlang quo_get_expr
#' @importFrom rlang is_syntactic_literal
#' @noRd
#' @keywords internal
expr_type <- function(x){
  if(quo_is_symbol(x)){
    "symbol"
  }else if(quo_is_call(x)){
    "call"
  }else if(is_syntactic_literal(quo_get_expr(x))){
    "literal"
  }else{
    typeof(x)
  }
}

#' Check whether symbols exist before they are parsed
#' @param x A (single) quosure
#' @importFrom rlang quo_get_expr
#' @importFrom rlang quo_get_env
#' @importFrom rlang eval_tidy
#' @importFrom rlang as_label
#' @noRd
#' @keywords internal
parse_symbol <- function(x){
  if(exists(quo_get_expr(x), where = quo_get_env(x))){
    result <- eval_tidy(x)
    if(inherits(result, "function")){ # special case for functions like 'data'
      as_label(x)                     # which exist in Global
    }else{
      result
    }
  }else{
    as_label(x)
  }
}

#' Internal, recursive function to parse a call
#' 
#' Note that most entries passed to `brew` will be of type `call`, making this 
#' quite an important function. For example, `brew(y == 1)` is a call, but so 
#' are functions, e.g. `brew(list(x = 1))`.
#' 
#' Importantly, when using NSE, values are checked to see if they are present
#' in the working environment, but names are not. So 
#' `x <- 1; y <- 10; brew(y == x)` will parse to `list(y = 1)` not 
#' `list(10 = 1)`
#' @importFrom rlang abort
#' @importFrom rlang as_quosure
#' @importFrom rlang as_string
#' @importFrom rlang eval_tidy
#' @importFrom rlang quo_get_expr
#' @importFrom rlang quo_get_env
#' @noRd
#' @keywords internal
parse_call <- function(x){
  y <- quo_get_expr(x)
  switch(as_string(y[[1]]),
         "==" = {
           if(length(y) != 3L){
             abort("Invalid argument passed to `brew()`")
           }
           result <- as_quosure(y[[3]], env = quo_get_env(x)) |>
                     switch_expr_type() |>
                     list()
           names(result) <- as_label(y[[2]])
           return(result)},
         eval_tidy(x) # parse anything else
       )
}