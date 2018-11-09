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
#' @param ... extra arguments can be supplied to get_stamenmap.
#' @import ggmap
#' @import ggplot2
#' @import ggrepel
#' @import tibble
#' @importFrom raster intersect
#' @importFrom dplyr mutate select left_join group_by summarise
#' @importFrom Conigrave check_names
#' @importFrom magrittr %>%
#' @importFrom utils setTxtProgressBar txtProgressBar globalVariables
#' @export iloc_map
#' @return A list containing a map of the ilocs, and suburb and iloc demographics

iloc_map = function(iloc,
                    maptype = "watercolor",
                    zoom = 12,
                    postcodes = T,
                    gap = .07,
                    map = T,
                    ...) {
  #START FUNCTION

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

  iloc_outline = fortify(iloc_f, region = "ILOC_NAME") %>% tibble::as.tibble()

  shp_tb =  fortify(shapes, region = "SSC_CODE16") %>% tibble::as.tibble() %>%
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
    suburb_data$postcodes = lapply(seq_along(suburb_data$suburb), function(x) {
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
    unique

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


  adjusted_demog = demographics %>%
    dplyr::select(
      IL_name,
      f_16_24 = f_15_24,
      m_16_24 = m_15_24,
      f_25_44,
      m_25_44,
      f_45_64,
      m_45_64,
      f_65_plus,
      m_65_plus
    ) %>%
    mutate(f_16_24 = round(f_16_24 * 8 / 9, 0),
           m_16_24 = round(m_16_24 * 8 / 9, 0))
adjusted_demog$total = rowSums(adjusted_demog[,-1],na.rm=T)

  # Get map base ####
  if (map == T) {
    map = get_stamenmap(
      bbox = c(
        left = min(shp_tb$long, na.rm = T) - gap,
        bottom = min(shp_tb$lat, na.rm = T) - gap,
        right = max(shp_tb$long, na.rm = T) + gap,
        top = max(shp_tb$lat, na.rm = T) + gap
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
          group = group,
          color = female_p
        ),
        size = .3
      ) +
      geom_path(
        data = iloc_outline,
        aes(x = long, y = lat, group = group),
        color = "black",
        size = .7
      ) +
      geom_label_repel(
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
      ) +
      scale_fill_gradient(low = "#efebe9", high = "#4a148c") +
      scale_color_gradient(low = "#0D47A1", high = "#F50057") +
      labs(
        fill = "population\nsize 15+\n",
        caption = paste0(
          "Note: total population = ",
          sum(suburb_data$persons, na.rm = T)
        ),
        color = "percent\nfemale",
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
    iloc_demog = demographics[, -1],
    iloc_demog_adj = adjusted_demog
  )

} #end of function

### Global variables defined so that dpylr will not throw errors
globalVariables(c(".","ILOC_NAME","SSC_CODE_2016","Indigenous_P_Tot_M",
                  "Indigenous_P_Tot_F","Indigenous_P_Tot_P","lat","long","name",
                  "females","males","total","suburb","women","men","persons",
                  "ILOC_CODE_2016","A_0_4_Indig_F","A_5_14_Indig_F","A_5_14_Indig_M",
                  "A_15_24_Indig_F","A_15_24_Indig_M",'A_25_44_Indig_M','A_45_64_Indig_F',
                  'A_45_64_Indig_M','A_65_over_Indig_F','A_65_over_Indig_M',"group","female_p",
                  "limit","lon","SSC_NAME16","A_0_4_Indig_M","A_25_44_Indig_F"))
