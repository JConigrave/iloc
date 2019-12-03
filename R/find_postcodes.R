#' find_postcodes
#'
#' takes in suburb names, outputs postcodes
#'
#' @param suburbs A character vector. The name of one, or more suburbs
#' @importFrom dplyr %>%
#' @importFrom rgeos intersect
#' @return A list containing a map of the ilocs, and suburb and iloc demographics
#' @export find_postcodes

find_postcodes = function(suburbs) {
  suburb_names = as.character(iloc::shape_ssc$SSC_NAME16)
  if (suburbs == "all") {
    #if all suburbs wanted, put them all in.
    suburbs = suburb_names
  }

  if (any(!suburbs %in% suburb_names)) {
    #check if there are any incorrectly spelt suburb names
    Conigrave::check_names(suburbs, suburb_names)
  }

  #keep desired suburbs -----------------------------------------------
  suburb_shapes = iloc::shape_ssc %>%
    subset(SSC_NAME16 %in% suburbs) #subset suburb shapes to those desired

  suburb_shapes@data$id = as.character(rownames(suburb_shapes@data)) #put ids from row names to a columm(why?)


  #store unique suburb namess -----------------------------------------
  sscnames = tibble::tibble(id = as.character(suburb_shapes@data$SSC_CODE16),
                            name = as.character(suburb_shapes@data$SSC_NAME16)) %>%
    unique

  sscnames =  fortify(suburb_shapes, region = "SSC_CODE16") %>% tibble::as_tibble() %>% #add suburb names
    left_join(sscnames, by = "id") %>%
    group_by(name) %>%
    summarise(lat = mean(lat, na.rm=-T), lon = mean(long, na.rm=T))

  #set up postcode shapes --------------------------------------------
  poa = iloc::shape_poa #store postal shapes

  #postcode grabbing function ----------------------------------------
  get_postcodes = function(suburb) {
    temp_suburb_shape = suburb_shapes %>%
      subset(SSC_NAME16 == as.character(suburb))
    areas = raster::intersect(temp_suburb_shape, poa)
    postcodes = as.character(areas@data$POA_NAME16) %>%
      unique

    return(paste(postcodes, collapse = "; "))
  }

  #start finding postcodes ------------------------------------------
  message("finding postcodes...")
  pb = txtProgressBar(min = 0,
                      max = length(sscnames$name),
                      style = 3)
  sscnames$postcode = lapply(seq_along(sscnames$name), function(x) {
    pc = get_postcodes(sscnames$name[x])
    setTxtProgressBar(pb, x)
    return(pc)
  }) %>%
    unlist
  close(pb)
  message("postcodes found")

  sscnames = sscnames %>%
    select(suburb = name,postcode,lon,lat)

  return(sscnames)

} #end of function

globalVariables(c("gender","n"))
