#' @export
#' 
#' @import ggplot2 
#' @importFrom rlang .data
#' 
#' @author Rachel Carroll <rachelcarroll4@@gmail.com>
#' 
#' @title Performance Analysis Plotting Function
#' 
#' @description Uses output from \code{performance()} to create a ggplot
#'  performance analysis visualization. 
#' 
#' @param  perfDF A data.frame object containing performance analysis results 
#' from \code{performance()}
#' @param  title The plot title
#' @param  subtitle The plot subtitle
#' @param  legend_name The legend title
#' @param  preferred_cand_races A character vector of the unique races contained 
#' in the \code{preferred_candidate} column of \code{perfDF}. This argument is
#' optional and is used with \code{colors} to indicate the color of the plot 
#' associated with the race preferences.
#' @param colors Plot colors for the voter race groups. Colors must 
#' be listed in the desired order with respect \code{preferred_cand_races} if 
#' arguments are used together.
#' @param  breaks Numeric vector containing x axis breaks
#' @param  lims Numeric vector containing x axis limits
#' @param  bar_size The size of plot bars. Passed to \code{geom_linerange()}. 
#' @param  position_dodge_width The width value passed to \code{position_dodge()}.  
#' Affects spacing between the plot bars.
#' @param  label_size The size of vote share labels
#' @param  cand_name_size Text size of candidate names if \code{includeCandName = TRUE}
#' @param  cand_name_pad Padding between candidate name and y axis if 
#' \code{includeCandName = TRUE}.
#' @param  contest_name_size Text size of contest name
#' @param  contest_name_pad Padding between contest name and y axis
#' @param  panel_spacing space between panels. This argument is relevant only if 
#' there are multiple jurisdictions in \code{perfDF}. 
#' @param panelBy Column name from perfDF passed to \code{facet_grid()} to 
#' create panels. Recommended options are Jurisdiction and Map. 
#' Defaults to Jurisdiction.  
#' @param  includeCandName Logical indicating if candidate names should appear 
#' on the left side of the plot.
#' @param  includeMeanDiff Logical indicating if the mean difference between 
#' preferred_candidate  across all elections should appear in the plot.
#' 
#' @seealso \link{performance}
# 
#' @return ggplot visualization of performance analysis
#' 
#' @examples
#'library(eiExpand)
#'data(example_performance_results)
#'performance_plot(example_performance_results)
#'
#'#ggplot2::ggsave("perf_plot.png", width = 12, height = 7)

performance_plot <- function(
  perfDF, 
  title = "Performance Analysis Results",
  subtitle = NULL,
  legend_name = "Preferred Candidate:",
  preferred_cand_races = NULL,
  colors = NULL,
  breaks = seq(0, 100, 20),
  lims = c(0,100),
  bar_size = 5,
  label_size = 4,
  position_dodge_width = .8,
  cand_name_size = 6,
  cand_name_pad = -1,
  contest_name_size = 20,
  contest_name_pad = NULL,
  panel_spacing = .7,
  panelBy = 'Jurisdiction',
  includeCandName = TRUE,
  includeMeanDiff = TRUE
) {
  
  #-------------------                   QC                  ------------------ 
  # set column names to lower case so case doesn't matter in panelBy argument
  colnames(perfDF) <- stringr::str_to_lower(colnames(perfDF))
  
  if( includeMeanDiff ){
    if ( length(unique(perfDF$preferred_candidate)) != 2 ){
      message(paste0("There must be 2 unique values in preferred_candidate ", 
                     "to calculate mean difference. ", 
                     "Setting includeMeanDiff = FALSE"))
      includeMeanDiff = FALSE
    }
  }
  
  #make sure panelBy is valid
  if( !is.null(panelBy) ) {
    
    panelBy <- stringr::str_to_lower(panelBy)
    if( !panelBy %in% c("map", "jurisdiction") ) {
      message("it is recommended to use Map or Jurisdiction in the panelBy argument")
    }
    
    if(!panelBy %in% colnames(perfDF)){
      stop("Invalid panelBy argument. Value must be a column from perfDF.")
    }
    
  }
  

  
  #-------------------    Prepare Data and Plot Attributes   ------------------ 
  
  #### Set panel field based on panelBy argument
  panel_field <- perfDF %>% dplyr::pull(panelBy)
  perfDF$panel_field <- panel_field
  
  
  #### adjust from decimal to percentage if needed
  if( max(perfDF$vote) <= 1 ){ perfDF$vote <- perfDF$vote * 100 }
  
  #### set factor on panel_field
  if( !inherits(perfDF$panel_field, "factor") ){
    perfDF$panel_field <- factor(
      perfDF$panel_field, 
      levels = c(perfDF$panel_field %>% unique %>% sort))
  }
  
  #### Set race and colors for plot
  race <- preferred_cand_races
  
  if( is.null(preferred_cand_races) ){
    race <- unique(perfDF$preferred_candidate)
  }
  
  # Colors
  if( is.null(colors) ){
    if( length(race) == 2){
      colors <- c(viridis::viridis(10)[4], viridis::viridis(10)[7])
    } else if(  length(race) == 3 ){
      colors <- c(viridis::viridis(10)[3], viridis::viridis(10)[6], 
                  viridis::viridis(10)[8]) 
    }
  }
  
  # Create color by race label
  race_colors <- colors
  names(race_colors) <- race
  
  # Convert preferred_candidate field into factor so plot will order 
  # according to preferred_cand_races argument
  perfDF$preferred_candidate <- perfDF$preferred_candidate %>% 
    factor(levels = rev(race))
  
  # Create yvar field (adds year to Contest field of not there already there)
  if( all(grepl("[0-9][0-9][0-9][0-9] .*", perfDF$contest)) ){
    perfDF$yvar <- perfDF$contest
  } else {
    perfDF$yvar <- paste(perfDF$year, perfDF$contest)
  }
  
  # set candidate name and contest name default location estimates
  if ( includeCandName ){
    if( is.null(contest_name_pad) ){
      # set contest_name_pad based on size on cand names
      maxcandname <- max(nchar(perfDF$candidate))
      contest_name_pad = maxcandname * 7.5
    }
  }
  
  #### Factor Contest/Cand Names
  # set factor on a grouped field for plot bar ordering
  # NOTE setting factor on Voter_Race alone doesnt change bar order. 
  #      Setting a ghost aes (fill) to the created  grouped variable, fillaescol, works.
  #      Order is important. Do not change or cand names might align incorrectly
  
  # arrange data based on levels created above
  perfDF <- perfDF %>% 
    dplyr::arrange(dplyr::desc(.data$year), .data$contest, 
                   .data$preferred_candidate, .data$candidate)
  
  # make grouped variables
  perfDF$fillaescol <- paste(perfDF$year, perfDF$contest, perfDF$candidate)
  perfDF$candnm_fillaescol <- paste(perfDF$year, perfDF$candidate, perfDF$contest)
  
  # set factor levels (levels will be in correct order since data is arranged above)
  fillaes_levels <- perfDF$fillaescol %>% unique
  candnm_fillaes_levels <- perfDF$candnm_fillaescol %>% unique
  
  #set factor
  perfDF$fillaescol <- factor(perfDF$fillaescol, levels = fillaes_levels)
  perfDF$candnm_fillaescol <- factor(perfDF$candnm_fillaescol,
                                   levels = candnm_fillaes_levels)
  
  # create labels to set fill (color) of all labels to white
  fill_labs <- rep("white", length(perfDF$fillaescol))
  names(fill_labs) <- perfDF$fillaescol
  
  # factor yvar 
  perfDF$yvar <- factor(perfDF$yvar, levels = perfDF$yvar %>% unique %>% rev)
  
  # Calculate mean differences
  if ( includeMeanDiff ){
    meandiff_lu <- perfDF %>%
      dplyr::group_by(.data$panel_field, .data$preferred_candidate) %>%
      dplyr::summarize(mean_vote = mean(.data$vote)) %>%
      tidyr::pivot_wider(
        names_from = "preferred_candidate",
        values_from = "mean_vote"
      ) %>%
       dplyr::ungroup() 
    
    meandiff_lu$mean_diff <- round(abs(meandiff_lu[[2]] - meandiff_lu[[3]]),1)
    
    meandiff_lu <- meandiff_lu %>% dplyr::select("panel_field", "mean_diff")
  
    if( length(unique(perfDF$panel_field)) == 1 ) {
      md <- unique(meandiff_lu$mean_diff)
      caption <- paste("Mean Diff:", md)
    } else {
        caption <- NULL
        panel_labs <- paste0(meandiff_lu$panel_field, 
                            "\nMean Diff: ",
                            meandiff_lu$mean_diff)
        names(panel_labs) <- meandiff_lu$panel_field
    }
  } else {
    caption <- NULL
    panel_labs <- levels(perfDF$panel_field)
    names(panel_labs) <- levels(perfDF$panel_field)
    
  } # END if includeMeanDiff
  
  
  #----------------------------    Build Plot   ---------------------------- 
  suppressWarnings({
    perf_plot <- ggplot(
      data = perfDF, 
      aes(x = .data$vote, y = .data$yvar,
          fill = .data$fillaescol, #need an aesthetic here so lines and labels dont stack over eachother
          label = .data$candidate,
          xmin = 0, xmax = .data$vote)
    ) +
      labs(title = title, 
           subtitle = subtitle,
           caption = caption,
           y = "", x = "Vote Share (%)")  +
      geom_linerange(aes(color = .data$preferred_candidate),
                     position = position_dodge(width = position_dodge_width), 
                     size = bar_size) +
      geom_label(aes(y = .data$yvar, label = round(.data$vote, 1), 
                     color = .data$preferred_candidate),
                 position = position_dodge(width = position_dodge_width),
                 size = label_size, 
                 show.legend = FALSE) 
  })
  
  if( includeCandName ){
    
    first_panel <- levels(perfDF$panel_field)[
      levels(perfDF$panel_field) %in% unique(perfDF$panel_field)][1]
    
    suppressWarnings({
      perf_plot <- perf_plot +
        geom_text(data = perfDF %>% dplyr::filter(.data$panel_field == first_panel),
                aes(x = cand_name_pad, 
                    y = .data$yvar, 
                    fill = .data$candnm_fillaescol,
                    label = .data$candidate),
                inherit.aes = FALSE,
                hjust = 1, position = position_dodge(width = position_dodge_width),
                family = "serif", fontface="italic",size = cand_name_size) 
    })
  } # END if includeCandName
  
  perf_plot <- perf_plot +
    scale_x_continuous(breaks = breaks, expand = expansion(mult = c(0, .1))) +
    coord_cartesian(xlim = lims, clip = "off") +
    scale_color_manual(name = legend_name, #rename legend title
                       values = race_colors) +
    scale_fill_manual(values = fill_labs) +
    theme_bw() +
    theme(
      text = element_text(family = "serif"),
      plot.title = element_text(size = 24, hjust = .5, face = "bold"),
      plot.subtitle = element_text(size = 20, hjust = .5, face = "italic"),
      plot.caption = element_text(size = 12, face = "italic"),
      #vote share axis
      axis.line.x = element_line(size = .5, color = "grey"),
      axis.title.x = element_text(size = 18, hjust = .5, family = "serif"), #% vote for candidate
      axis.text.x = element_text(size = 15, family = "serif"),
      #contest axis
      axis.text.y = element_text(size = contest_name_size, hjust = .5, 
                                 family = "serif", face="bold"),
      axis.ticks.y = element_blank(),
      legend.position = "bottom",
      legend.title =element_text(size=11, face="bold"),
      legend.text = element_text(size=11, face="bold"),
      strip.text.x = element_text(size=12), #panel text size
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.spacing = unit(panel_spacing, "lines")
    )
  
  # add faced grid if more than one Jurisdiction
  if( length(unique(perfDF$panel_field)) > 1 ){
    
    perf_plot <- perf_plot +  
      facet_grid(. ~ panel_field, labeller = as_labeller(panel_labs))
  }
  
  # if including cand name, move contest names over so they dont overlap
  if ( includeCandName ){
    perf_plot <- perf_plot + 
      theme(
        axis.text.y = element_text(margin = margin(r = contest_name_pad))
      )
  }
  
  ##### Return plot
  return(perf_plot)
}

