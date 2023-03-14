#' @export
#' 
#' @importFrom dplyr mutate
#' 
#' @author Rachel Carroll <rachelcarroll4@@gmail.com>
#' @author Loren Collingwood <lcollingwood@@unm.edu>
#' 
#' @title Performance Analysis Calculation
#' 
#' @description Performance Analysis calculates election outcomes of past 
#' contests given hypothetical voting district(s). This analysis has been used to 
#' determine if a Gingles III violation occurs due to how a district map is drawn.
#' It can also be used to demonstrate that a more equitable alternative map exists.
#' This function assumes RPV so it should only be used with contests where RPV 
#' has been established.
#'
#' @param data A data.frame object containing precinct-level election results 
#' for contests of interest. It must include candidate vote counts and contest 
#' total votes fields and must be subsetted to the relevant precincts. This 
#' data.frame will likely be the output of a "Split Precinct Analysis". 
#' @param cands A character vector of the candidate vote counts field names 
#' from \code{data} that are relevant to the given year and contest being analyzed.
#' @param candidate A character vector of candidate names. The names must be listed 
#' in the same order \code{cands}. The values will appear in the output 
#' data.frame exactly as they are written in this argument.
#' @param preferred_candidate A character vector of preferred racial groups 
#' associated with the candidates. The values must be listed in the correct 
#' order with respect to the \code{cands}/\code{candidate} arguments. The values
#' will appear in the output data.frame exactly as they are written in this 
#' argument. 
#' @param total A character vector of the the contest total vote count field names 
#' from \code{data}.
#' @param contest The name of the contest being analyzed
#' @param year The year of the contest being analyzed	
#' @param election_type The election type the contest being analyzed (e.g 
#' "General" or "Primary") 
#' @param jurisdiction String containing the name of the jurisdiction being 
#' analyzed (i.e a district number or "County"). Be sure that \code{data} is 
#' subsetted only to this jurisdiction.
#' @param map String containing the name of the district map being 
#' analyzed (e.g "remedial" or "adopted"). This is an optional field that 
#' defaults to blank.
#' @param includeTotal Boolean indicating if a total number of votes row should 
#' be appended to the output data.frame
#'
#' @return data.frame of Performance Analysis results by candidate
#' 
#' @examples
#' 
#' library(eiExpand)
#' data(south_carolina)
#' 
#' # Get sample election data
#' D5_election <- south_carolina %>%
#'    dplyr::filter(District == 5)
#' 
#' # Run performance Analysis on 2018 Governor contest
#' perf_results <- performance(
#'   data = D5_election,
#'   cands = c("R_mcmaster", "D_smith"), 
#'   candidate = c("McMaster (R)", "Smith (D)"), # formatted candidate names
#'   preferred_candidate = c("White", "Black"), # race preference of candidates respectively
#'   total = "total_gov",
#'   contest = "Governor",
#'   year = 2018,	
#'   election_type = "General",	
#'   jurisdiction = "District 5"
#' ) 

performance <- function(
  data = NULL,
  cands = "", # list of candidate field names from data
  candidate = "",	#candidate names SAME ORDER AS CANDS
  preferred_candidate = "", # race preferred candidate SAME ORDER AS CANDS 
  total = "",
  contest = "",
  year = "",	
  election_type = "",
  map = "",
  jurisdiction = "",
  includeTotal = FALSE
) {
  
  # Message about the importance argument orders
  message("IMPORTANT: make sure arguments 'candidate' and 'preferred_candidate'
          are in correct order respective to 'cands' argument")
  
  # Warning if there are missing values in any of the cand fields
  if( sum(is.na(data[,cands])) != 0 ){
    warning("there are missing values in data")
  }
  
  # Sum total votes 
  tot_vote <- sum(data[[total]], na.rm=T)
  
  # Calculate performance for each cand
  perfcalcs <- sapply(
    cands, 
    function(x){ sum(data[[x]], na.rm=T) / tot_vote }
  )
  
  # Create performance DF 
  out <- data.frame(Vote = perfcalcs) %>% 
    round(3) %>%
    mutate(   # add in relevant plotting fields based on function arguments
      Candidate	 = candidate,
      Preferred_Candidate = preferred_candidate,
      Map = map,
      Jurisdiction = jurisdiction,
      Election_Type	 = election_type,
      Year = as.numeric(year),
      Contest = contest,
      .before = 1
    )
  
  # Add totals row is wanted
  if ( includeTotal ){
    
    out <- rbind(
      out, 
      c("Total Votes", NA, jurisdiction, election_type, year, contest, tot_vote)
    )
    
    rownames(out)[length(cands) + 1] <- total
    
  }
  
  # Return data 
  return(out)
  
} # End function
