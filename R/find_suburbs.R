#' find_suburbs
#'
#' takes in suburb names, outputs postcodes
#'
#' @param postcodes A character vector. The name of one, or more postcodes
#' @importFrom dplyr %>%
#' @importFrom rgeos intersect
#' @return A list containing a map of the ilocs, and suburb and iloc demographics
#' @export find_suburbs

find_suburbs = function(postcodes = NULL) {

  postcodes = trimws(sprintf("%04d", na.omit(postcodes)))
  postcods = postcodes[postcodes != "NA"]


  postcode_shapes = iloc::shape_poa

  all_postcodes = as.character(iloc::shape_poa$POA_NAME16)

  if(is.null(postcodes)) {
    message("Finding ALL SUBURBS!!")
    p = all_postcodes
  }else{
    p = postcodes
  }

  #check all postcodes are OK
  if(!all(p %in% all_postcodes)) warning("Mystery postcodes: ",paste(p[!p %in% all_postcodes], collapse = ", "), call. = F)

  #keep desired suburbs -----------------------------------------------
  postcode_shapes = postcode_shapes %>%
    subset(POA_NAME16 %in% p) #subset suburb shapes to those desired

  #set up postcode shapes --------------------------------------------
  burbs = iloc::shape_ssc #store subrub shapes
  ilocs = iloc::shape_iloc


  #postcode grabbing function ----------------------------------------
  get_suburbs = function(poa) {
    message(poa)
    shape_i = postcode_shapes %>%
      subset(POA_NAME16 == poa)

    sub_areas = raster::intersect(burbs, shape_i)

    # get data per region
    sub_tbl = data.table::data.table(ggplot2::fortify(sub_areas, region = "SSC_NAME16"))
    sub_size = data.frame(locality = as.character(sub_areas$SSC_NAME16),
                           area = geosphere::areaPolygon(sub_areas),
                          state = sub_areas$STE_NAME16)

    sub_size$area = sub_size$area / sum(sub_size$area)

    iloc_areas = raster::intersect(ilocs, shape_i)
    iloc_tbl = data.table::data.table(ggplot2::fortify(iloc_areas, region = "ILOC_NAME"))

    iloc_size = data.frame(locality = as.character(iloc_areas$ILOC_NAME),
                           area = geosphere::areaPolygon(iloc_areas),
                           state = iloc_areas$STATE_NAME)
    iloc_size$area = iloc_size$area / sum(iloc_size$area)

    sub_tbl = sub_tbl[, .(
      "postcode" = poa,
      long = mean(long),
      lat = mean(lat),
      type = "SSC"
    ) , .("locality" = id)][sub_size, on = "locality"]

    iloc_tbl = iloc_tbl[, .(
      "postcode" = poa,
      long = mean(long),
      lat = mean(lat),
      type = "ILOC"
    ) , .(locality = id)][iloc_size, on = "locality"]

    all_dat = rbind(sub_tbl, iloc_tbl)
    all_dat$area_pc = round(all_dat$area*100,4)

    all_dat = all_dat[order(type, area_pc),
                      .(postcode, locality, type, area_pc, state, long, lat)]

    return(all_dat)
  }

  #start finding postcodes ------------------------------------------
  message("finding postcodes...")
  pb = txtProgressBar(min = 0,
                      max = length(p),
                      style = 3)
  out_postcodes = lapply(seq_along(p), function(x) {
    pc = get_suburbs(p[x])
    setTxtProgressBar(pb, x)
    return(pc)
  }) %>%
    do.call(rbind, .)
  close(pb)
  message("postcodes found")

  return(out_postcodes)

} #end of function

globalVariables(c("gender","n", "postcode","POA_NAME16","sscnames"))

#' data_dictionary
#'
#' open data dictionary
#' @export data_dictionary

data_dictionary = function(){

  shell.exec(system.file("data_dictionary.html", package = "iloc"))

  }

