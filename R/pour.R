#' Retrieve a specified slot from options
#' 
#' UI based on `{here}`; i.e. uses strings separated by commas
#' 
#' @param ... strings: what slots should be returned
#' @param slot_name Optional manual override to default slot. Useful for
#' ensuring no clashes when used within a package
#' @importFrom rlang trace_back
#' @return a vector
#' 
#' @export

pour <- function(..., .slot, .pkg){

  if(missing(.slot) & missing(.pkg)){
    package_check <- trace_back()$namespace |> 
                     check_within_pkg()
  
    if(package_check$within){
      pour_package(..., .pkg = package_check$pkg)
    }else{
      pour_interactive(...)
    }
  }else{
    if(missing(.slot)){ # i.e. .pkg is given, but not .slot
      pour_package(..., .pkg = .pkg)
    }else{ # .slot is given, but not .pkg
      pour_interactive(..., .slot = .slot)
    }
  }
}

# pour data for a selected package
pour_package <- function(..., .pkg){
  dots <- unlist(list(...))
  .data <- check_pour_package(.pkg)
  if(length(dots) > 0){
    check_is_character(dots, fill = FALSE)
    search_down(.data, dots)
  }else{
    return(.data)
  }
}

# pour data for a selected slot
pour_interactive <- function(..., .slot){
  dots <- unlist(list(...))
  .data <- check_pour_interactive(.slot)
  if(length(dots) > 0){
    check_is_character(dots, fill = FALSE)
    search_down(.data, dots)
  }else{
    return(.data)
  }
}

# internal, recursive function to do the searching
search_down <- function(x, lookup_strings){
  
  ## print output: for testing purposes only
  # cat(paste0("strings: ", paste(unlist(lookup_strings), collapse = " | "), "\n"))
  
  if(is.null(names(x))){ # skip levels without names
    if(length(x) < 1){ # if nothing below that level, return empty vector (NULL)
      c()
    }else{
      search_down(do.call(c, x), lookup_strings)
    }
  }else{
    lookup <- x[names(x) == lookup_strings[1]]
    result <- do.call(c, lookup)
    names(result) <- unlist(lapply(lookup, names))
  
    if(length(lookup_strings) <= 1){
      unlist(result)
    }else{
      search_down(result, lookup_strings[-1])
    }
  }
}

## ALTERNATIVE IMPLEMENTATIONS
## simple option; unlist
## note that this is probably inefficient
# lookup <- unlist(dots) |> paste(collapse = ".")
# y <- unlist(x)
# result <- y[grepl(lookup, names(y))]
# if(length(result) < 1){
#   NULL
# }else{
#   names(result) <- NULL
#   result
# }

## alternative: use parse
# string <- paste0("x |> ",
#   paste(
#     paste0("getElement(dots[[", seq_along(dots), "]])"),
#     collapse = " |> "))
# eval(parse(text = string))
# # only returns one element

## original working example
# for(i in seq_along(dots)){
#   x <- getElement(x, dots[i]])
# }
# x
## only returns one element