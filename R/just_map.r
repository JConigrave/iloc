#' map_only
#'
#' Just grabs a map based on provided ilocs
#' @param iloc name of iloc
#' @param maptype what map type do you want?
#' @param zoom smaller number is lower resolution
#' @param gap area around bbox to apply padding
#' @param left_adjust change left side
#' @param bottom_adjust change bottom
#' @param right_adjust you get it
#' @param top_adjust as above.
#' @param ...

map_only = function(iloc = "Port Adelaide",
                    maptype = "watercolor",
                    zoom = 5,
                    gap = .07,
                    left_adjust = 0,
                    bottom_adjust = 0,
                    right_adjust = 0,
                    top_adjust = 0,
                    ...) {
  #START FUNCTION
  iloc = iloc_find(iloc) #get all matches

  ##Helper function(s)

  #check names
  Conigrave::check_names(iloc, as.character(iloc::iloc_names[[1]])) #first we check to see if the target area is spelled correctly.

  iloc_f = iloc::shape_iloc %>%
    subset(ILOC_NAME %in% iloc) #subset iloc shapes to desired iloc

  shp_tb = ggplot2::fortify(iloc_f, region = "ILOC_NAME") %>% tibble::as_tibble()

  #--------------------------- add column totals


  # Get map base ####
  map = ggmap::get_stamenmap(
    bbox = c(
      left = min(shp_tb$long, na.rm = T) - gap + left_adjust,
      bottom = min(shp_tb$lat, na.rm = T) - gap + bottom_adjust,
      right = max(shp_tb$long, na.rm = T) + gap + right_adjust,
      top = max(shp_tb$lat, na.rm = T) + gap + top_adjust
    )
    ,
    maptype = maptype,
    zoom = zoom,
    ...
  )

  return(ggmap::ggmap(map))
}
