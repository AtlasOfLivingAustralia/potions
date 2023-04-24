update_package_names <- function(data, pkg){
  if(!any(data$mapping$packages == pkg)){
    data$mapping$packages <- unique(c(data$mapping$packages, pkg))
  }
  return(data)
}

update_default_name <- function(data, .slot){
  data$mapping$current_slot <- .slot
  return(data)
}

update_package_data <- function(data, provided, pkg){
  if(is.null(provided)){
    return(data)
  }else{
    data$packages[[pkg]] <- update_list(
      initial = data$packages[[pkg]],
      update = provided)
    return(data)
  }
}

update_slot_data <- function(data, provided, .slot){
  # browser()
  if(is.null(provided)){
    return(data)
  }else{
    data$slots[[.slot]] <- update_list(
      initial = data$slots[[.slot]],
      update = provided)
    return(data)
  }
}

# internal function to update lists with information given by the user
update_list <- function(initial, update){
  if(is.null(initial)){
    update
  }else{
    c(initial[!(names(initial) %in% names(update))], update)
  }
}