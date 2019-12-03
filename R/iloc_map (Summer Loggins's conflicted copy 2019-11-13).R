#' iloc_map
#'
#' iloc_map takes in a vector of ILOC names, and returns a map and demographic information.
#'
#' @param iloc A charcter vector. The name of one, or more ILOC regions
#' @param maptype see get_stamenmap
#' @param zoom numeric. Defines the resolution of the map. Lower resolution loads faster
#' @param postcodes bool. If True, postcodes will be output for suburbs.
#' @param gap numeric. Adds this to long and lat. Larger numbers result in larger displayed area
#' @param map bool. If TRUE, this function will output a map.
#' @param label bool. If TRUE \(default\) the largest suburbs will be labelled.
#' @param left_adjust a numeric.
#' @param bottom_adjust a numeric.
#' @param right_adjust a numeric.
#' @param top_adjust a numeric.
#' @param ... extra arguments can be supplied to get_stamenmap.
#' @import ggmap
#' @import ggplot2
#' @import ggrepel
#' @import tibble
#' @importFrom raster intersect
#' @importFrom dplyr mutate select left_join group_by summarise filter rename
#' @importFrom reshape2 melt
#' @importFrom Conigrave check_names
#' @importFrom magrittr %>%
#' @importFrom utils setTxtProgressBar txtProgressBar globalVariables
#' @export iloc_map
#' @return A list containing a map of the ilocs, and suburb and iloc demographics

# iloc = "Wollong"
# maptype = "watercolor"
# zoom = 12
# postcodes = T
# gap = 0.07
# map = T
# label = T
# left_adjust = 0
# bottom_adjust = 0
# right_adjust = 0
# top_adjust = 0

iloc_map = function(iloc = "Port Adelaide",
                    maptype = "watercolor",
                    zoom = 12,
                    postcodes = T,
                    gap = .07,
                    map = T,
                    label = T,
                    left_adjust = 0,
                    bottom_adjust = 0,
                    right_adjust = 0,
                    top_adjust = 0,
                    ...) {
  #START FUNCTION
  iloc = iloc_find(iloc) #get all matches

  ##Helper function(s)
  find_postcodes = function(suburb, simple_shapes, simple_poa) {
    shape_suburb = simple_shapes %>%
      subset(SSC_NAME16 == as.character(suburb))

    areas = raster::intersect(shape_suburb, simple_poa)
    postcodes = areas@data$POA_NAME16 %>%
      unique
    return(paste(postcodes, collapse = "; "))
  }

  #check names
  Conigrave::check_names(iloc, as.character(iloc::iloc_names[[1]])) #first we check to see if the target area is spelled correctly.

  iloc_f = iloc::shape_iloc %>%
    subset(ILOC_NAME %in% iloc) #subset iloc shapes to desired iloc

  shapes = raster::intersect(iloc_f, iloc::shape_ssc) #keep subrub polygons inside the iloc
  shapes@data$id = rownames(shapes@data) #put ids from row names to a columm

  if (postcodes == T) {
    #if we are searching for postcodes, set up the shapefile
    poa = raster::intersect(shapes, iloc::shape_poa)
  }

  sscnames = tibble::tibble(id = shapes@data$SSC_CODE16,
                            name = shapes@data$SSC_NAME16) %>%
    unique #save remaining suburb names.

  iloc_outline = ggplot2::fortify(iloc_f, region = "ILOC_NAME") %>% tibble::as_tibble()

  shp_tb =  ggplot2::fortify(shapes, region = "SSC_CODE16") %>% tibble::as.tibble() %>%
    left_join(sscnames, by = "id") #add suburb names to shape_tble

  ssc_data_f = iloc::data_ssc %>%
    dplyr::select(
      id = SSC_CODE_2016,
      males = Indigenous_P_Tot_M,
      females = Indigenous_P_Tot_F,
      total = Indigenous_P_Tot_P
    ) %>%
    mutate(id = gsub("SSC", "", id))

  shp_tb = left_join(shp_tb, ssc_data_f, by = "id")
  shp_tb$female_p = round((shp_tb$females / shp_tb$total) * 100, 1)


  ##get suburb data ready
  x = shp_tb %>%
    dplyr::select(ssc = id, lat, long, suburb = name)
  suburb_data = shp_tb %>%
    dplyr::select(
      suburb = name,
      women = females,
      men = males,
      persons = total
    ) %>%
    unique %>%
    left_join(x, by = "suburb") %>% unique %>%
    group_by(suburb, women, men, persons) %>%
    summarise(lat = mean(lat), lon = mean(long))

  if (postcodes == T) {
    message("finding postcodes...")
    pb = txtProgressBar(
      min = 0,
      max = length(suburb_data$suburb),
      style = 3
    )
    suburb_data$postcode = lapply(seq_along(suburb_data$suburb), function(x) {
      pc = find_postcodes(suburb_data$suburb[x],
                          simple_shapes = shapes,
                          simple_poa = poa)
      setTxtProgressBar(pb, x)
      return(pc)
    }) %>%
      unlist
    close(pb)
    message("postcodes found")
    suburb_data = suburb_data[, c(1:4, 7, 5, 6)]
  }

  # prepare iloc demographic table
  demographics = tibble(IL_code = iloc_f@data$ILOC_CODE,
                        IL_name = iloc_f@data$ILOC_NAME) %>%
    unique %>%
    mutate(IL_code = as.character(IL_code),
           IL_name = as.character(IL_name))

  data_iloc = iloc::data_iloc %>%
    dplyr::select(
      IL_code = ILOC_CODE_2016,
      f_0_4 = A_0_4_Indig_F,
      m_0_4 = A_0_4_Indig_M,
      f_5_14 = A_5_14_Indig_F,
      m_5_14 = A_5_14_Indig_M,
      f_15_24 = A_15_24_Indig_F,
      m_15_24 = A_15_24_Indig_M,
      f_25_44 = A_25_44_Indig_F,
      m_25_44 = A_25_44_Indig_M,
      f_45_64 = A_45_64_Indig_F,
      m_45_64 = A_45_64_Indig_M,
      f_65_plus = A_65_over_Indig_F,
      m_65_plus = A_65_over_Indig_M
    ) %>%
    #mutate(IL_code = gsub("ILOC","",IL_code)) %>%
    as.data.frame
  data_iloc$total = rowSums(data_iloc[, 2:ncol(data_iloc)], na.rm = T)


  demographics = left_join(demographics, data_iloc, by = "IL_code")

  grab_gender = function(g, gender) {
    gender_columns = names(demographics)[grep(g, names(demographics))]
    demographics[, c("IL_name", gender_columns)] %>%
      reshape2::melt("IL_name") %>%
      rename(n = value, age = variable) %>%
      mutate(gender = gender,
             age = gsub(paste0(g, "_"), "", age))
  }

  demographics = rbind(grab_gender("m","males"), grab_gender("f","females")) %>%
    select(IL_name,gender,age,n)

  adjusted_demog = demographics %>%
    filter(!age %in% c("0_4","5_14")) %>%
    mutate(age = ifelse(age == "15_24","16_24",age))
  adjusted_demog$n[adjusted_demog$age == "16_24"] = adjusted_demog$n[adjusted_demog$age == "16_24"] *(9/10)
adjusted_demog$n = adjusted_demog$n %>% round(0)

adjusted_demog = adjusted_demog %>%
  mutate(age = gsub("_","-",age)) %>%
  mutate(age = gsub("65-plus","65+",age))
demographics = demographics %>%
  mutate(age = gsub("_","-",age)) %>%
  mutate(age = gsub("65-plus","65+",age))
#--------------------------- add column totals


  # Get map base ####
  if (map == T) {
    map = get_stamenmap(
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

    # place polygons on map and label


    final_map = ggmap(map) +
      geom_polygon(data = shp_tb,
                   aes(
                     x = long ,
                     y = lat,
                     group = group,
                     fill = total
                   ),
                   alpha = .8) +
      geom_path(
        data = shp_tb,
        aes(
          x = long ,
          y = lat,
          group = group#,
          #color = female_p
        ),
        size = .3
      ) +
      geom_path(
        data = iloc_outline,
        aes(x = long, y = lat, group = group),
        color = "black",
        size = .7
      )

      if(label){
     final_map = final_map + geom_label_repel(
          data = suburb_data %>%
            data.frame() %>%
            mutate(limit = as.numeric(as.character(scale(
              persons
            )))) %>%
            dplyr::filter(limit > 1.5),
          aes(x = lon, y = lat, label = suburb),
          direction = "y",
          nudge_x = -0.0125,
          hjust = 1,
          size = 2,
          alpha = 0.85
        )
      }
      final_map = final_map +
      scale_fill_gradient(low = "#efebe9", high = "#4a148c") +
      #scale_color_gradient(low = "#0D47A1", high = "#F50057") +
      labs(
        fill = "population\nsize 15+\n",
        caption = paste0(
          "Note: total population = ",
          sum(suburb_data$persons, na.rm = T)
        ),
        #color = "percent\nfemale",
        x = " ",
        y = " "
      ) +
      theme(axis.line = element_line(color = "black"),
            plot.caption = element_text(hjust = 0)) +
      ggtitle(" ")
  } else{
    final_map = NULL
  }

  list(
    map = final_map,
    suburb_demog = suburb_data,
    iloc_demog = demographics,
    iloc_demog_adj = adjusted_demog,
    shape_table= shp_tb
  )

} #end of function

#'iarea_demog
#'
#'iarea demographics deatils
#'@param variables variables to search for
#'@param iarea indigenous area
#'@importFrom data.table %like%
#'@importFrom dplyr %>%
#'@export iarea_demog

iarea_demog = function(..., iarea = "IARE401016"){

all_demogs = iloc::data_iarea

variables = tidyselect::vars_select(as.character(unique(all_demogs$variable)),...)

demographics = iloc::data_iarea %>%
   filter(IARE_CODE_2016 %in% iarea)

variables_rows = unlist(lapply(variables, function(v){
  which(demographics$variable == unlist(v))
}))

demographics = demographics[variables_rows,]

demographics$variable = names(variables)
demographics

}


### Global variables defined so that dpylr will not throw errors
globalVariables(c(".","ILOC_NAME","SSC_CODE_2016","Indigenous_P_Tot_M",
                  "Indigenous_P_Tot_F","Indigenous_P_Tot_P","lat","long","name",
                  "females","males","total","suburb","women","men","persons",
                  "ILOC_CODE_2016","A_0_4_Indig_F","A_5_14_Indig_F","A_5_14_Indig_M",
                  "A_15_24_Indig_F","A_15_24_Indig_M",'A_25_44_Indig_M','A_45_64_Indig_F',
                  'A_45_64_Indig_M','A_65_over_Indig_F','A_65_over_Indig_M',"group","female_p",
                  "limit","lon","SSC_NAME16","A_0_4_Indig_M","A_25_44_Indig_F","id","IL_name",
                  "f_15_24", "f_16_24", "f_25_44", "f_45_64", "f_65_plus", "m_15_24", "m_25_44", "m_45_64", "m_65_plus",
                  "m_16_24","shp_tb","value","IL_code","variable","age"))
