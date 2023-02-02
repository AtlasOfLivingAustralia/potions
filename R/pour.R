#' Retrieve a specified slot from options
#' 
#' UI based on here; i.e. uses strings separated by commas
#' 
#' @param ... strings: what slots should be returned
#' @return a vector
#' 
#' @export

pour <- function(...){
  
  dots <- list(...)
  slot_name <- getOption("potions_slot_name")
  
  if(length(dots) > 0){
    # check dots are strings
    dots_check <- unlist(lapply(dots, function(a){inherits(a, "character")}))
    if(any(!dots_check)){
      stop("all arguments to `pour` must have class `character`")
    }
    
    # recursively search downwards
    search_down(getOption(slot_name), unlist(dots))
  
  # if no args given, return whole object 
  }else{
    getOption(slot_name)
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