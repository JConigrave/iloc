#' iloc_find
#'
#' Allows a user to input the start of an iloc name. All matching names are returned.
#' @param x A character vector. The start of the name of one, or more ILOC regions
#' @export iloc_find
#' @return a tibble containing the names of all ILOCs
#'
iloc_find = function(x){

  names = as.character(iloc::iloc_names[[1]])
  as.character(names[grepl(x,names)])

}
