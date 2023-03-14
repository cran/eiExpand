#' @export
#' @importFrom rlang .data
#' 
#' @author Rachel Carroll <rachelcarroll4@@gmail.com>
#' 
#' @title Calculate percent land area intersections
#' 
#' @description Calculates the percent area 
#' intersection between the geometries in a sf data.frame and a 
#' single boundary shape. 
#'
#' @param sfdf sf dataframe with one or more geometries. 
#' @param shp sf dataframe with a single shape boundary. 
#' 
#' @return sfdf dataframe with pct_intersect column

percent_intersect <- function(sfdf, shp) {
  #   -----------------------     QC      ----------------------- 
  # make sure created fields (pct_intersect and tempid are not already present
  if("pct_intersect" %in% colnames(sfdf)){
    stop("sfdf argument already contains a field called pct_intersect")
  }
  
  if("tempid" %in% colnames(sfdf)){
    stop("remove or rename tempid column from sfdf argument")
  }
  
  # make sure there is only one shape in shp
  if(nrow(shp) > 1) {
    stop("shp must be an sf with one row containing a single boundry shape")
  }
  
  #   -----------------------   data prep      -----------------------  
  # set to planar
  sfdf <- sfdf %>% sf::st_transform(crs = 26915) 
  shp <- shp %>% sf::st_transform(crs = 26915) 
  
  # unique id field
  sfdf <- sfdf %>% dplyr::mutate(tempid = dplyr::row_number()) 
  
  #   --------------      get fully contained rows      --------------  
  potential_include <- sfdf[sf::st_intersects(sfdf, shp, sparse = F)[,1],]
  
  if( nrow(potential_include) == 0 ){ # if there are no overlapping shapes
    message("There are no overlapping geometries in sfdf")
    result_sfdf <- sfdf %>%
      dplyr::mutate(pct_intersect = 0) %>%
      dplyr::select(-"tempid")
    
  } else { # If there are at least one include shp
    covered <- sf::st_covered_by(potential_include, shp, sparse = F)[,1]
    
    complete <- potential_include[covered,] %>%
      dplyr::mutate(pct_intersect = 1) %>%
      sf::st_drop_geometry() %>%
      dplyr::select("tempid", "pct_intersect")
    
    #   --------      calculate percent vap partially intersected     ---------  
    calcs <- potential_include[!covered,] %>%
      dplyr::mutate(full_block_area =  sf::st_area(.data$geometry)) 
    
    calcs <- sf::st_intersection(
      calcs, 
      shp %>% dplyr::select("geometry")
    ) %>%
      dplyr::mutate(
        intersect_area = sf::st_area(.data$geometry),
        pct_intersect = round(as.numeric(.data$intersect_area/.data$full_block_area),4)
      ) %>%
      sf::st_drop_geometry() %>% 
      dplyr::select("tempid", "pct_intersect")
    
    #   ----------------        bind results together       ---------------  
    calcs <- rbind(complete, calcs) 
    
    #   --------     join in percent intersect to original data     ---------  
    # join in pct_intersect
    result_sfdf <- dplyr::left_join(sfdf, calcs, by = "tempid") %>%
      dplyr::select(-"tempid")
    
    result_sfdf$pct_intersect[is.na(result_sfdf$pct_intersect)] <- 0
  
  } #END if nrow > 0 
  
  #   ----------------     return    ----------------- 
  return(result_sfdf)

}


