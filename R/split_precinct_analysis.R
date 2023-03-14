#' @export
#' 
#' @importFrom dplyr mutate filter
#' @importFrom rlang .data
#' 
#' @author Rachel Carroll <rachelcarroll4@@gmail.com>
#' @author Loren Collingwood <lcollingwood@@unm.edu>
#' 
#' @title Split precinct analysis - VAP Adjusted Election Data
#' 
#' @description Run Split Precinct Analysis using precinct-level geometries and 
#' election data, a district shape, and block-level vap data. This function 
#' calculates the percent vap of a precinct contained in the district boundary
#' of interest. Then, if specified, multiplies election vote counts by percent
#' vap.
#'
#' @param vtd A sf dataframe with precinct-level geometries potentially in the 
#' district from \code{planShp}. For election adjustments, it should also 
#' contain election results in columns defined in \code{vote_col_names}.
#' @param planShp A sf dataframe with one row containing district plan shape  
#' boundary (one district).
#' @param block_pop_data A sf object of blocks covering the region, with vap 
#' column 
#' @param vote_col_names Character vector containing the name of the columns to 
#' be adjusted based on percent vap. This should include election results 
#' columns names in \code{vtd}. 
#' @param lower_thresh A decimal. If the percent area of a precinct
#' inside the \code{planShp} is equal to or below this threshold, the precinct
#' will be removed. Defaults to .02.
#' @param upper_thresh A decimal. If the percent area of a precinct
#' inside the \code{planShp} is equal to or above this threshold, the precinct
#' will be considered to be fully contained in the district. Defaults to .98.
#' @param keepOrigElection A boolean indicating if original election vote counts
#' should be preserved in the output dataset for comparison purposes.
#' @param generatePlots Boolean indicating if function should generate
#' a list of map checking plots. If TRUE, the function output will include a list 
#' of plots that show split precincts and intersecting blocks within and outside 
#' of the district. 
#' @param ggmap_object A ggmap object of the area on interest to be the background
#' of plots if \code{generatePlots = TRUE}. If this argument is not specified, 
#' plots will be generated without a map background.
#' @param verbose A boolean indicating whether to print out status messages.
#' 
#' @return If \code{generatePlots = FALSE}, returns a split precinct results
#' data.frame with vap percentages and adjusted election data. If 
#' \code{generatePlots = TRUE}, returns a list with the result data.frame in the 
#' first element and the list of plots in the second. 
#' 
#' @examples 
#' \donttest{
#' library(eiExpand)
#' library(sf)
#' 
#' # load data and shps
#' data(planShp); data(vtd); data(mt_block_data)
#' 
#' # filter to a few vtds for this example
#' vtd <- vtd %>% 
#'   dplyr::filter(
#'     GEOID20 %in% c("30091000002", "30085000012", "30085000018", "30085000010")
#'   )
#' 
#' # run split precinct analysis without plots
#' spa_results <- split_precinct_analysis(
#'   vtd = vtd, 
#'   planShp = planShp, 
#'   block_pop_data = mt_block_data,
#'   vote_col_names = c('G16HALRZIN', 'G16HALDJUN', 'G16HALLBRE', 
#'                      'G16GOVRGIA', 'G16GOVDBUL', "G16GOVLDUN"),
#'   keepOrigElection = TRUE,
#'   generatePlots = FALSE) 
#' 
#' # run with plots
#' spa_list <- split_precinct_analysis(
#'   vtd = vtd, 
#'   planShp = planShp, 
#'   block_pop_data = mt_block_data,
#'   vote_col_names = c('G16HALRZIN', 'G16HALDJUN', 'G16HALLBRE', 
#'                      'G16GOVRGIA', 'G16GOVDBUL', "G16GOVLDUN"),
#'   lower_thresh = 0,
#'   keepOrigElection = TRUE,
#'   generatePlots = TRUE) 
#' 
#' # View results
#' spa_list[["results"]]
#' 
#' # View plots
#' #library(gridExtra)
#' #do.call("grid.arrange", c(spa_list[["plots"]], ncol=1))
#' }

split_precinct_analysis <- function(
    vtd, 
    planShp, 
    block_pop_data,
    vote_col_names = NULL,
    lower_thresh = .02,
    upper_thresh = .98,
    keepOrigElection = TRUE,
    generatePlots = FALSE,
    ggmap_object = NULL,
    verbose = FALSE
    ) {
  
  #----------------------------         QC         ----------------------------#

  # make sure block data has vap column
  colnames(block_pop_data) <- stringr::str_to_lower(colnames(block_pop_data))
  if( !"vap" %in% colnames(block_pop_data) ) {
    stop("argument block_pop_data must contain a column named vap")
  }
  
  # make sure only doing one district boundary at a time
  if(nrow(planShp) > 1) {
    stop("planShp must be an sf with one row containing a district boundry of interest")
  }
  
  # warning if no vote cols are specified
  if(is.null(vote_col_names)) {
    warning(paste("argument vote_col_names is NULL. Therefore, pct_vap will",
    "be calculated but no election results adjustments will be made"))
  }
  
  # make sure vote_cols are vtd data
  if( length(setdiff(vote_col_names,colnames(vtd))) > 0 ) {
    stop("there are values in vote_col_names that are not in vtd")
  }
  
  # make sure there isnt already pct_intersect and pct_vap fields
  if("pct_vap" %in% colnames(vtd)){
    stop("vtd already has a field pct_vap")
  }
  
  if("pct_intersect" %in% colnames(vtd)){
    stop("vtd already has a field pct_intersect")
  }
  
  # make sure upper_thresh is reasonable
  if(upper_thresh > 1) stop("invalid value: upper_thresh cannot be greater than 1")
  if(upper_thresh < 0) stop("invalid value: upper_thresh cannot be less than zero")
  if(upper_thresh < .9) message("It is recommended to keep upper_thresh between .9 and 1")
  
  
  # make sure lower_thresh is reasonable
  if(lower_thresh > 1) stop("invalid value: lower_thresh cannot be greater than 1")
  if(lower_thresh < 0) stop("invalid value: lower_thresh cannot be less than zero")
  if(lower_thresh > .1) message("It is recommended to keep lower_thresh between 0 and .1")
  
  #----------------------------     Prep Data      ----------------------------#
  # set all to planar
  orig_vtd_crs <- sf::st_crs(vtd)
  vtd <- vtd %>% sf::st_transform(crs = 26915) 
  planShp <- planShp %>% sf::st_transform(crs = 26915) 
  block_pop_data <- block_pop_data %>% sf::st_transform(crs = 26915) 
  
  # add original election results fields if keepOrigElection = TRUE
  if( keepOrigElection ){
    # store original election counts
    orig_election <- vtd[,vote_col_names] %>% sf::st_drop_geometry()
    colnames(orig_election) <- paste0(paste0(colnames(orig_election), "_orig"))
    #bind to vtd
    vtd <- cbind(vtd, orig_election)
  }
  
  # Subset precincts to those possibly within the districts
  include_precs <- percent_intersect(vtd, planShp) %>%
    suppressWarnings() %>%
    filter(.data$pct_intersect > lower_thresh)
  
  if( nrow(include_precs) == 0 ) {
    stop("There are no vtds that intersect with planShp")
  }
  
  # Separate split precincts from completely included precincts
  split_precincts <- include_precs %>% 
    filter(.data$pct_intersect < upper_thresh)
  
  complete_precincts <- include_precs %>% 
    filter(.data$pct_intersect >= upper_thresh) %>%
    mutate(pct_vap = 1)
  
  # Subset blocks to those possibly within any of the included precincts
  blockIntersects <- suppressWarnings(
    sf::st_intersects(block_pop_data, include_precs, sparse = F) ) 
  blockIntersects <- rowSums(blockIntersects) > 0L
  include_blocks <-block_pop_data[blockIntersects,]

  #-------------------------     Prep Plot Items      -------------------------#
  if (generatePlots) {
    
    # initialize plot storage lists
    plot_list <- list()
    plot_elements <- list()
    
    # ggplot starting object
    if( !is.null(ggmap_object) ) { mapstart <- ggmap::ggmap(ggmap_object)
    } else mapstart <- ggplot2::ggplot()

    # district shape
    plot_elements[["shp"]] <- ggplot2::geom_sf(data = planShp,
                       size = 1, col = "black",fill = "transparent",
                       inherit.aes = F)
    # #vtds
    # plt_vtds <- ggplot2::geom_sf(data = split_precincts, 
    #                             size = .4, col = "turquoise", fill="transparent",
    #                             inherit.aes = F) 
    
  }
  
  #---------------------     Split Precinct Analysis      ---------------------#
  
  # initialize results list
  adjusted_precinct_votes <- list()

  # loop through split precs
  if( nrow(split_precincts ) > 0) {
    for (i in 1:nrow(split_precincts)) {
      
      if(verbose) print(paste("calculating precinct", i))
      
      ##### Select Split Precinct
      
      precinct_to_look <- split_precincts %>% dplyr::slice(i)
      
      ##### Select Blocks fully within precincts
      
      precinct_blocks <- percent_intersect(include_blocks, precinct_to_look) %>%
        suppressWarnings() %>%
        filter(.data$pct_intersect >= .5) %>%
        dplyr::select(-"pct_intersect")
      
      ##### Calculate percent VAP of the precinct within the district
      if( nrow(precinct_blocks) == 0 ){
        
        if(verbose){
          message(paste(
            "precinct", i, "does not contain any blocks.",
            "Setting pct_vap to the percent precinct area inside the district"))
        }

        multiplier <- precinct_to_look$pct_intersect
  
      } else if( nrow(precinct_blocks) > 0 ){
        
        # Take only Blocks fully inside the precinct and district  
        prec_blocks_inside <- percent_intersect(precinct_blocks, planShp) %>%
          suppressWarnings() %>%
          filter(.data$pct_intersect >= .5)
        
        # Part of Precinct Inside District VAP 
        num <- sum(prec_blocks_inside$vap, na.rm=T)
        
        # VAP over full precinct
        denom <-sum(precinct_blocks$vap, na.rm=T)
        
        # Percent of VAP multiplier in this split precinct that is inside the district 
        if( denom == 0 ) {multiplier <- 0} else {multiplier <- num/denom}
        
      }
  
      #####   Apply multiplier to election result columns from vote_col_names  #####    
      if( !is.null(vote_col_names) ){
        # multiply vote cols by the multiplier
        multiply_vars <- precinct_to_look %>%
          dplyr::select(tidyselect::all_of(vote_col_names)) %>%
          sf::st_drop_geometry() %>%
          mutate(dplyr::across(tidyselect::where(is.numeric), ~ .x * multiplier))
        
        # replace vote cols with the adjusted vote cols
        precinct_to_look[,vote_col_names] <- multiply_vars
      }
      
      # Tag on VAP multiplier percent
      precinct_to_look <- precinct_to_look %>%
        dplyr::mutate(pct_vap = multiplier, .before = "geometry")
      
      ##### If specified, generate map checks
      if (generatePlots) {
        # vtd
        plot_elements[["vtd"]][[i]] <- ggplot2::geom_sf(
          data = precinct_to_look, 
          size = .4, col = "turquoise", fill="transparent",
          inherit.aes = F) 
        # vtd blocks outside district
        plot_elements[["blockOut"]][[i]]  <- ggplot2::geom_sf(
          data = precinct_blocks,
          size = .05, col = "pink", fill="transparent",
          inherit.aes = F)
        # vtd blocks inside district
        plot_elements[["blockIn"]][[i]] <- ggplot2::geom_sf(
          data = prec_blocks_inside,
          size = .05, fill = "green", alpha = .4,
          inherit.aes = F) 
        
        # build and store plot just for this vtd
        plot_list[[i]] <- mapstart + 
          plot_elements[["shp"]] + 
          plot_elements[["vtd"]][[i]] + 
          plot_elements[["blockOut"]][[i]] + 
          plot_elements[["blockIn"]][[i]] +
          ggtitle(paste("Split Precinct:", i))
      }
      
      #####   Store this iteration in a list
      adjusted_precinct_votes[[i]] <- precinct_to_look
      
    } # close i loop through split precincts
  } # close if nrow(split_precincts) > 0
  #---------------------   Combine and return results     --------------------#
  
  if( nrow(split_precincts) > 0 ){
    # split precinct results
    out <- dplyr::bind_rows(adjusted_precinct_votes)
    # bind in complete precs
    out <- rbind(
      out, 
      complete_precincts %>% dplyr::select(colnames(out))
    )
  } else { out <- complete_precincts}

  #return to original crs
  out <- out %>% sf::st_transform(crs = orig_vtd_crs) 
  
  if( generatePlots ){
    #### Create full map and append to plot list
    plot_all <- mapstart + plot_elements[["shp"]]
    for( i in seq_along(plot_list) ) {
      plot_all <- plot_all +
        plot_elements[["vtd"]][[i]] +
        plot_elements[["blockOut"]][[i]] +
        plot_elements[["blockIn"]][[i]] 
    }
    plot_all <- plot_all + ggtitle("All Split Precincts")
    plot_list[[1 + length(plot_list)]] <- plot_all
  }

  #---------------------  Return results     --------------------#

  # if generating plots output is a list 
  if( generatePlots ){ 
    res <- list("results" = out, "plots" = plot_list )
    } else res <- out
  
  return(res)
} 
