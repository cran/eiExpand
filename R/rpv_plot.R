#' @export
#' 
#' @import ggplot2
#' @importFrom rlang .data
#' 
#' @author Rachel Carroll <rachelcarroll4@@gmail.com>
#' @author Loren Collingwood <lcollingwood@@unm.edu>
#' @author Kassra Oskooii  <kassrao@@gmail.com>
#' 
#' @title Racially Polarized Voting Analysis (RPV) Plot
#' 
#' @description Creates a custom visualization of RPV results
#'
#' @param rpvDF A data.frame containing RPV results
#' @param title The plot title
#' @param subtitle The plot subtitle
#' @param legend_name The legend title
#' @param voter_races A vector of the unique voter races contained in the 
#' \code{Voter_Race} column of \code{rpvDF}. This argument will set the order 
#' in which voter races are displayed in the plot and legend. Can be used with 
#' \code{colors}, to indicate the which color of the plot to associate with each  
#' voter race.
#' @param colors Defines the plot colors for the voter race groups. Colors must 
#' be listed in the desired order with respect \code{voter_races} if arguments 
#' are used together.
#' @param position_dodge_width The width value indicating  spacing between the 
#' plot bars. Passed to \code{position_dodge()}.  
#' @param bar_size The size of plot bars. Passed to \code{geom_linerange()}. 
#' @param label_size The size of RPV estimate label
#' @param contest_name_size Text size of contest name
#' @param cand_name_size Text size of candidate names if 
#' \code{includeCandName = TRUE}
#' @param contest_name_pad Padding between contest name and y axis
#' @param cand_name_pad Padding between candidate name and y axis if 
#' \code{includeCandName = TRUE}.
#' @param contest_sep String indicating how to separate contest. Options "s", 
#' "shade", or "shading" shade the background of every other contest. 
#' Options "l", "line", "lines" create light grey double lines between contests.
#' @param shade_col color to shade contest separation bands when
#' \code{contest_sep = "s"}. Defaults to light grey.
#' @param shade_alpha alpha parameter passed to \code{geom_tile()} to indicate 
#' transparency of contest separation bands when \code{contest_sep = "s"}
#' @param panel_spacing Space between facet grid panels
#' @param breaks Numeric vector containing x axis breaks
#' @param lims Numeric vector containing x axis limits
#' @param includeErrorBand Logical indicating if the confidence interval band 
#' should appear on the plot. If \code{TRUE}, the RPV estimate labels will
#' appear in the middle of each bar instead of at the ends so they don't cover 
#' the error bands.
#' @param  includeCandName Logical indicating if candidate names should appear on
#' the left side of the plot.
#' @param panelBy Column name from rpvDF passed to \code{facet_grid()} to create panels. 
#' @param txtInBar Logical indicating location of the RPV estimate labels. If, TRUE,
#' estimates will be in the middle of the plot bars. If FALSE, they will be at the end
#' of the bars. 
#' 
#' @return Bar plot visualization of RPV analysis as a ggplot2 object
#' 
#' @examples
#' \donttest{
#'library(eiExpand)
#'data(example_rpvDF)
#'
#'# Note that these plots are designed to be 
#'# saved as a png using ggplot2::ggsave(). See first example for recommending 
#'# sizing, noting that height and weight arguments may need adjusting 
#'# depending on plot attributes such as number of contests and paneling 
#'
#'# plot county-level results with all defaults
#'rpvDF_county <- example_rpvDF %>% dplyr::filter(Jurisdiction == "County")
#'rpv_plot(rpvDF_county)
#'
#'# save to png with recommended sizing
#'# ggplot2::ggsave("rpv_plot_default.png", height = 10, width = 15)
#'
#'# include CI bands
#'rpv_plot(rpvDF_county, includeErrorBand = TRUE)
#'
#'# include CI bands with estimate labels outside bar
#'rpv_plot(
#'   rpvDF_county, 
#'   includeErrorBand = TRUE,
#'   txtInBar = FALSE
#')
#'
#'# panel by preferred candidate
#' rpvDF_county$Year <- paste(rpvDF_county$Year,
#'                            "\n") # so contest and year are on different lines
#' rpvDF_county$Preferred_Candidate <- paste(rpvDF_county$Preferred_Candidate, 
#'                                           "\nPreferred Candidate")
#' rpv_plot(
#'   rpvDF_county,
#'   panel_spacing = 6,
#'   panelBy = "Preferred_Candidate"
#' )
#' 
#'# plot all jurisdictions with panels
#'rpv_plot(example_rpvDF, panelBy = "Jurisdiction")
#'# add contest separation shading
#'rpv_plot(
#'   example_rpvDF, 
#'   panelBy = "Jurisdiction",
#'   contest_sep = "s"
#')
#'
#'# plot panels by voter_race and remove legend
#'rpv_plot(rpvDF_county,
#' panel_spacing = 6, 
#' panelBy = "Voter_Race") + 
#'   ggplot2::theme(legend.position="none")
#'   
#'}


##### Begin Function #####
rpv_plot <- function(
  rpvDF = NULL,
  title = "Racially Polarized Voting Analysis Results",
  subtitle = "Estimated Vote for Candidates by Race",
  legend_name = "Voters' Race:",
  voter_races = NULL,
  colors = NULL,
  position_dodge_width = .8,
  bar_size = NULL,
  label_size = 4,
  contest_name_size = 20,
  cand_name_size = 6,
  contest_name_pad = NULL,
  cand_name_pad = -1.5,
  contest_sep = NULL,
  shade_col = "grey75",
  shade_alpha = 0.1,
  panel_spacing = NULL,
  breaks = seq(0, 100, 20),
  lims = c(0,110),
  includeErrorBand = FALSE,
  includeCandName = TRUE,
  panelBy = NULL,
  txtInBar = NULL
) {
  
  # -----------------------------  QC CHECKS   ----------------------------- 
  ##### First prep rpvDF and args to standardize  
  # make colnames, panelBy, contest_sep to all lowercase
  colnames(rpvDF) <- stringr::str_to_lower(colnames(rpvDF))
  if( !is.null(panelBy) ) panelBy <- stringr::str_to_lower(panelBy)
  if( !is.null(contest_sep) ) contest_sep <- stringr::str_to_lower(contest_sep)
  
  # make voter race and preferred candidate fields proper case
  rpvDF$preferred_candidate <- stringr::str_to_title(rpvDF$preferred_candidate)
  rpvDF$voter_race <- stringr::str_to_title(rpvDF$voter_race)
  
  ##### QC Checks
  # make sure correct column names are in rpvDF 
  expected_cols <- c('original_name', 'model', 'jurisdiction', 
                     'preferred_candidate', 'election_type', 'year', 'contest', 'candidate', 
                     'voter_race', 'estimate', 'lower_bound', 'upper_bound')
  
  missing_cols <- paste(setdiff(expected_cols, colnames(rpvDF)))
  
  if( length(missing_cols) > 0 ) {
    warning(
      paste("The following expected columns are missing from rpvDF:", 
            paste(missing_cols, collapse = ", ")))
  }
  
  # warning message if there is more than one model or jurisdiction but not paneling
  n_models <- rpvDF$model %>% unique %>% length()
  n_jurs <- rpvDF$jurisdiction %>% unique %>% length()
  
  if( is.null(panelBy) ){
    if(n_models > 1){
      warning("Data contains more than one model. It is recommended to plot one model at a time or use argument panelBy = 'Model'")
    }
    if(n_jurs > 1){
      warning("Data contains more than one jurisdiction It is recommended to plot one model at a time or use argument panelBy = 'Jurisdiction'")
    }
  } else {# if panelby is not null
    if(n_models > 1 & panelBy != "model"){
      warning("Data contains more than one model. It is recommended to plot one model at a time or use argument panelBy = 'Model'")
    }
    if(n_jurs > 1 & panelBy != "jurisdiction"){
      warning("Data contains more than one jurisdiction It is recommended to plot one model at a time or use argument panelBy = 'Model'")
    }
  }
  
  # unless paneling by race pref, candidate names should always be included
  if( !includeCandName ){
    if( !is.null(panelBy) ){
      if( panelBy != "preferred_candidate"){
        warning("It is recommended to include Candidate Names, consider using includeCandName = TRUE")
      }
    }
  }
  
  # Make sure panelBy by exists in data
  if( !is.null(panelBy) ){
    if( !panelBy %in% colnames(rpvDF) ) stop("'panelBy' must be a column name in rpvDF")
  }
  
  
  # ---------------------------PREPARE DATA   ----------------------------- 
  ####  Initialize plot data frame
  pdat <- rpvDF
  
  #### Concatenate year and contest for Y axis if not done already in rpvDF
  if( all(grepl("[0-9][0-9][0-9][0-9] .*", pdat$contest)) ){
    pdat$yvar <- pdat$contest
  } else {
    pdat$yvar <- paste(pdat$year, pdat$contest)
  }
  
  #### Factor Race/Pref Cand    #### 
  # for plot ordering of legend/bars and panels
  # get unique values of race fields
  v_race_unique <- pdat$voter_race %>% unique %>% sort
  pc_race_unique <- pdat$preferred_candidate %>% unique %>% sort
  
  # set factor on preferred candidate
  pdat$preferred_candidate <- factor(pdat$preferred_candidate, 
                                     levels =  pc_race_unique)
  
  # set factors levels for Voter_Race if not specified
  if( is.null(voter_races) ) {
    # alphabetical order if voter_races not specified
    voter_races <- v_race_unique
  }
  
  # set factor for Voter_Race
  voter_races <- stringr::str_to_title(voter_races)
  pdat$voter_race <- factor(pdat$voter_race, levels = rev(voter_races))
  
  # set same factor to Preferred_Candidate if it contains the same
  # values as Voter_Race
  if( length(v_race_unique) == length(pc_race_unique) ){
    if( all(v_race_unique == pc_race_unique) ) {
      pdat$preferred_candidate <- factor( pdat$preferred_candidate, 
                                          levels = voter_races)
    }
  }
  
  #### Factor Contest/Cand Names    #### 
  # set factor on a grouped field for plot bar ordering
  # NOTE setting factor on Voter_Race alone doesnt change bar order. 
  #      Setting a ghost aes (fill) to the created  grouped variable, fillaescol, works.
  #      Order is important. Do not change or cand names might align incorrectly
  
  # arrange data based on levels created above
  pdat <- pdat %>% 
    dplyr::arrange(dplyr::desc(.data$year), .data$contest, .data$preferred_candidate, 
                   .data$candidate, .data$voter_race)
  
  # make grouped variables
  pdat$fillaescol <- paste(pdat$year, pdat$contest, pdat$candidate, pdat$voter_race)
  pdat$candnm_fillaescol <- paste(pdat$year, pdat$candidate, pdat$contest)
  
  # set factor levels (levels will be in correct order since data is arranged above)
  fillaes_levels <- pdat$fillaescol %>% unique
  candnm_fillaes_levels <- pdat$candnm_fillaescol %>% unique
  
  #set factor
  pdat$fillaescol <- factor(pdat$fillaescol, levels = fillaes_levels)
  pdat$candnm_fillaescol <- factor(pdat$candnm_fillaescol,
                                   levels = candnm_fillaes_levels)
  
  # factor yvar 
  pdat$yvar <- factor(pdat$yvar, levels = pdat$yvar %>% unique %>% rev)
  
  #### Colors  #### 
  # colors by race
  len_race <- length(v_race_unique)
  if( is.null(colors) ){
    if( len_race == 2){ 
      colors <- c(viridis::viridis(10)[4], viridis::viridis(10)[7]) 
    } else if( len_race == 3 ){
      colors <- c(viridis::viridis(10)[3], viridis::viridis(10)[6], 
                  viridis::viridis(10)[8]) 
    } else {  
      colors <- viridis::viridis(len_race * 2)[c(TRUE, FALSE)] 
    }
  }
  
  # label colors by race 
  race_colors <- colors
  names(race_colors) <- voter_races
  
  #color labels for fill (all white)
  #fill_valls_unique <- paste(pdat$contest, pdat$candidate, pdat$voter_race)
  fill_valls_unique <-  pdat$fillaescol
  fill_labs <- rep("white", length(fill_valls_unique))
  names(fill_labs) <- fill_valls_unique
  
  #### Panel Field   #### 
  
  # add panel field to data using field specified by "panelBy"
  
  if( !is.null(panelBy) ){
    
    # add panel field to data
    #panel_field <- pdat[,panelBy] %>% unlist(use.names=FALSE) %>% as.character()
    panel_field <- pdat %>% dplyr::pull(panelBy)
    pdat$Panel_Field <- panel_field
    
    # label for panel titles
    # NOTE make this not redundant 
    panel_names <- panel_field %>% unique 
    
    if( panelBy == "preferred_candidate" ){
      panel_labs <- panel_names
    }  else { panel_labs <- panel_names }
    
    names(panel_labs) <- panel_names
    
  } else {pdat$Panel_Field <- ""}
  
  # add factor to panel field if not already present
  if( !inherits(pdat$Panel_Field, "factor") ){
    pdat$Panel_Field <- factor(
      pdat$Panel_Field, 
      levels = pdat$Panel_Field %>% unique %>% sort
    )
  }

  
  
  # -------------------------- CREATE PLOT ELEMENTS --------------------------
  
  ##### Set defaults   ##### 
  
  # bar_size default
  if( is.null(bar_size) ) {
    if( includeErrorBand ) bar_size <- 7 else bar_size <- 5
  }
  
  # contest_name_pad default
  if ( is.null(contest_name_pad) ) {
    if( includeCandName ) {
      max_cand_length <- sapply(as.character(rpvDF$candidate), nchar) %>% 
        as.numeric %>% max
      contest_name_pad <- max_cand_length * 7.5
    } else {contest_name_pad = 0}
  }
  
  # panel_spacing defaults
  if( is.null(panel_spacing) ){
    
    panel_spacing <- 1 # usually one unless below conditions are met
    
    if( !is.null(panelBy) ) {
      if( includeCandName & panelBy == "preferred_candidate" ) {
        panel_spacing <- 5
      } 
    } 
  }
  
  #txtInBar default to true if not specified and using error bar
  if( is.null(txtInBar) ) {
    if( includeErrorBand ) {txtInBar <- TRUE} else {txtInBar <- FALSE}
  }
  
  #####  Bars (linerange)    ##### 
  
  ggplot_estimate_bars <- geom_linerange(
    aes(xmin = 0, 
        xmax = .data$estimate), 
    position = position_dodge(width = position_dodge_width), 
    size = bar_size
  ) 
  
  ##### Estimate Labels (geom_label or geom_text)   ##### 
  
  # with CI use txtInBar option 
  
  if(txtInBar) {
    # text in bar
    ggplot_est_label <- 
      geom_text(aes(x = .data$estimate/2,
                    label = round(.data$estimate, 1)
      ),
      position = position_dodge(width = position_dodge_width),
      size = label_size, color = "white",
      family = "serif", fontface = "bold",
      show.legend = FALSE) 
  } else{
    if( includeErrorBand ){
      # text at end of bar and ci band
      ggplot_est_label <- 
        geom_text(aes( x = .data$upper_bound + 1,
                       label = round(.data$estimate, 1)
        ),
        hjust = 0,
        position = position_dodge(width = position_dodge_width),
        size = label_size, color = "black",
        family = "serif", fontface = "bold",
        show.legend = FALSE) 
    } else { 
      # boxed label at end of bar
      ggplot_est_label <-       
        geom_label(aes(label = round(.data$estimate, 1)),
                   position = position_dodge(width = position_dodge_width),
                   size = label_size,
                   family = "serif", hjust = 0,
                   show.legend = FALSE) 
    } # END  if( includeErrorBand )
  }
  
  ##### Panels   ##### 
  if( !is.null(panelBy) ){
    ggplot_facet_grid <-  facet_grid(.~.data$Panel_Field, 
                                     labeller = as_labeller(panel_labs)) 
  } else { ggplot_facet_grid <- NULL }
  
  ##### Confidence Intervals  ##### 
  
  if( includeErrorBand ) {
    ggplot_error_bar <- geom_errorbarh(aes(xmin = .data$lower_bound, 
                                           xmax = .data$upper_bound),
                                       position = position_dodge(width = position_dodge_width),
                                       color = "black", 
                                       height = 0.3) 
  } else {ggplot_error_bar <- NULL}
  
  ##### Candidate Names / Contest names (y axis) format   ##### 

  if( includeCandName ){
    
    # create separate df to use in geom_text 
    candNm_DF <- pdat
    
    # subset candNm_DF data if panelling
    if( !is.null(panelBy) ){
      if( panelBy != "preferred_candidate" ) {
        # levels(pdat$Panel_Field)[1] should work usually but added this to make sure 
        # levels are also in the data bc sometimes levels are inherited from larger dataset 
        # then subset 
        first_panel <- levels(pdat$Panel_Field)[levels(pdat$Panel_Field) %in% unique(pdat$Panel_Field)][1]
        
        candNm_DF <- pdat %>% filter(.data$Panel_Field == first_panel)
        
      }
    }  
    
    ggplot_cand_names <- 
      geom_text(data = candNm_DF, 
                aes( x = cand_name_pad, 
                     y = .data$yvar,
                     fill = .data$candnm_fillaescol,
                     label = .data$candidate),
                hjust = 1, position = position_dodge(width = position_dodge_width),
                size = cand_name_size,
                family = "serif", fontface="italic",
                inherit.aes = FALSE) %>% 
      suppressWarnings() # suppress "Ignoring unknown aesthetics: fill"
    
    y_text_theme <- element_text(size = contest_name_size, hjust = .5,
                                 face="bold", family = "serif",
                                 margin = margin(r = contest_name_pad))
  } else{
    
    ggplot_cand_names <- NULL
    
    y_text_theme <- element_text(size = contest_name_size, hjust = 1, 
                                 face="bold", family = "serif")
  }
  
  ##### Contest Separation  ##### 
  #add shading location and color cols
  if( !is.null(contest_sep) ){
    #get number of unique contests
    ncontest <- pdat$yvar %>% unique() %>% length
    contest_sep <- stringr::str_to_lower(contest_sep)
    
    pdat <- pdat %>% dplyr::group_by(.data$yvar) %>%
      mutate( contest_value = dplyr::cur_group_id() ) 
    
    # lines between contests
    if( contest_sep %in% c("l", "line", "lines") ) {
      
      hline_locs <- seq(ncontest - 1) + .5
      hline_locs <- c(hline_locs + .01, hline_locs - .01)
      
      ggplot_contest_sep <- 
        geom_hline(yintercept = hline_locs,
                   color = "gray65",
                   size = 1)
      
      # shading every other contest area
    } else if( contest_sep %in% c("s", "shade", "shading") ) {
      
      pdat <- pdat %>% 
        mutate(
          shade_x = max(lims)/2, 
          rect_shade = ifelse( (.data$contest_value %% 2) == 0, "grey75", "white" )
        )
      
      ggplot_contest_sep <-
        geom_tile(aes(x = .data$shade_x), 
                  fill = pdat$rect_shade, 
                  alpha = shade_alpha,
                  size = 0, height = 1, width = max(lims), 
                  show.legend = FALSE) 
      
    } 
  } else { ggplot_contest_sep <- NULL } # END if !is.null(contest_sep)
  
  
# ----------------------------- BUILD PLOT  -----------------------------
  
  #### Initialize plot
  plot <- ggplot(data = pdat, 
                 aes(x = .data$estimate, 
                     y = .data$yvar,
                     fill = .data$fillaescol,
                     color = .data$voter_race)
  ) +
    labs(title = title, 
         subtitle = subtitle,
         x = "% Vote for Candidate", 
         y = "") 
  
  
  ##### Add ggplot elements from above
  plot <- plot +
    ggplot_contest_sep +
    ggplot_estimate_bars +
    ggplot_est_label +
    ggplot_facet_grid +
    ggplot_error_bar +
    ggplot_cand_names 
  
  ##### Finish plot setting final fonts etc
  plot <- plot + 
    scale_x_continuous(breaks = breaks, expand = expansion(mult = c(0, 0))) +
    scale_y_discrete(expand = expansion(mult = 0) ) +
    coord_cartesian(xlim = lims, clip = "off") +
    scale_color_manual(name = legend_name, 
                       values = race_colors)  +
    scale_fill_manual(values = fill_labs) +
    #guides(fill = "none" ) +
    theme_bw() +
    theme( 
      #Title/subtitle
      plot.title = element_text(size = 30, hjust = .5, face="bold", family = "serif"),
      plot.subtitle = element_text(size = 26, hjust = .5, face="italic", family = "serif"),
      # Contest Name
      axis.text.y  = y_text_theme,
      # x axis (% Vote)
      axis.title.x = element_text(size = 24, hjust = .5, family = "serif"), #% vote for candidate
      axis.text.x = element_text(size = 20, family = "serif"),
      # Legend
      legend.position = "bottom",
      legend.title =element_text(size = 24, face="bold", family = "serif"),
      legend.text = element_text(size = 24, face="bold", family = "serif"),
      # Panel
      strip.text.x = element_text(size=24, face="bold",family = "serif"),
      strip.text.y = element_blank(),
      
      panel.grid.minor.y =  element_blank() ,
      panel.grid.major.y = element_blank() ,
      #panel.border = element_blank(),
      panel.spacing.x = unit(panel_spacing, "lines"),
      panel.spacing.y = unit(.5, "lines")
    )
  
  #### Show plot
  plot
  
} # END rpv_plot function
