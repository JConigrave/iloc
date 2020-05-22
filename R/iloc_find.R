#' iloc_find
#'
#' Allows a user to input the start of an iloc name. All matching names are returned.
#' @param x A character vector. The start of the name of one, or more ILOC regions
#' @export iloc_find
#' @return a tibble containing the names of all ILOCs
#'
iloc_find = function(x){

  names = as.character(iloc::iloc_names[[1]])

  #escape special characters
  x = gsub("\\(","\\\\(" , x)
  x = gsub("\\)","\\\\)" , x)

  out <- sapply(x, function(i){
    as.character(names[grepl(i,names)])
  })

  unique(unlist(out))


}
