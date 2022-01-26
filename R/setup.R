
# DETAILS -----------------------------------------------------------------

#' Setup function for running all other Skillalytics R functions
#' 
#' This ensures proper setup of seasons selected to scrape and team name/ids for scraping 
#' and organizing datasets. 
#' @param seas Which seasons would you like to retrieve data for? No default set.
#' @keywords skillalytics
#' @export
#' @examples
#' skillalytics_setup()


# SETUP -------------------------------------------------------------------

skillalytics_setup <- function(seas){
  
  # SEASONS -----------------------------------------------------------------
  
    # Require Packages
    paks <- c("plyr", "tidyverse", "rvest" , "tictoc", "crayon", "lubridate")
    suppressWarnings(suppressMessages(lapply(paks, require, character.only = TRUE)))
    
    # Get Max Year
    curr_yr <- format(Sys.Date(), "%Y")
    
    # Available Years by Table
    # Regular
    sbs_yrs <- c(1918:2004, 2006:curr_yr)
    sas_yrs <- c(2008:curr_yr)
    stoi_yrs <- c(2008:curr_yr)
    sms_yrs <- c(1918:2004, 2006:curr_yr)
    gbs_yrs <- c(1918:2004, 2006:curr_yr)
    gso_yrs <- c(2009:curr_yr)
    tbl_yrs <- c(1918:2004, 2006:curr_yr)
    res_yrs <- c(1918:2004, 2006:curr_yr)
    # Postseason
    sbps_yrs <- c(1918:1919, 1921:2004, 2006:curr_yr)
    saps_yrs <- c(2008:curr_yr)
    sptoi_yrs <- c(2008:curr_yr)
    gbps_yrs <- c(1918:1919, 1921:2004, 2006:curr_yr)
    resp_yrs <- c(1918:1919, 1921:2004, 2006:curr_yr)
    
    # Create Year Availability Matrix
    all_yrs <- c(1918:curr_yr)
    yrs_mat <- data.frame(Year = all_yrs)
    yrs_mat <- yrs_mat %>%
      mutate(
        # Regular
        SBS = ifelse(Year %in% sbs_yrs, 1, 0),
        SAS = ifelse(Year %in% sas_yrs, 1, 0),
        STOI = ifelse(Year %in% stoi_yrs, 1, 0),
        SMS = ifelse(Year %in% sms_yrs, 1, 0),
        GBS = ifelse(Year %in% gbs_yrs, 1, 0),
        GSO = ifelse(Year %in% gso_yrs, 1, 0),
        TBL = ifelse(Year %in% tbl_yrs, 1, 0),
        RES = ifelse(Year %in% res_yrs, 1, 0),
        # Postseason
        SBPS = ifelse(Year %in% sbps_yrs, 1, 0),
        SAPS = ifelse(Year %in% saps_yrs, 1, 0),
        SPTOI = ifelse(Year %in% sptoi_yrs, 1, 0),
        GBPS = ifelse(Year %in% gbps_yrs, 1, 0),
        RESP = ifelse(Year %in% resp_yrs, 1, 0),
      )
    
    # Years Selected
    sel_yrs <- data.frame(Years = seas)
    
    # Years to Scrape
    # Regular
    sbs_yrs <- yrs_mat %>% subset(Year %in% sel_yrs$Years & SBS == 1) %>% select(Year) %>% pull(1)
    sas_yrs <- yrs_mat %>% subset(Year %in% sel_yrs$Years & SAS == 1) %>% select(Year) %>% pull(1)
    stoi_yrs <- yrs_mat %>% subset(Year %in% sel_yrs$Years & STOI == 1) %>% select(Year) %>% pull(1)
    sms_yrs <- yrs_mat %>% subset(Year %in% sel_yrs$Years & SMS == 1) %>% select(Year) %>% pull(1)
    gbs_yrs <- yrs_mat %>% subset(Year %in% sel_yrs$Years & GBS == 1) %>% select(Year) %>% pull(1)
    gso_yrs <- yrs_mat %>% subset(Year %in% sel_yrs$Years & GSO == 1) %>% select(Year) %>% pull(1)
    tbl_yrs <- yrs_mat %>% subset(Year %in% sel_yrs$Years & TBL == 1) %>% select(Year) %>% pull(1)
    res_yrs <- yrs_mat %>% subset(Year %in% sel_yrs$Years & RES == 1) %>% select(Year) %>% pull(1)
    # Postseason
    sbps_yrs <- yrs_mat %>% subset(Year %in% sel_yrs$Years & SBPS == 1) %>% select(Year) %>% pull(1)
    saps_yrs <- yrs_mat %>% subset(Year %in% sel_yrs$Years & SAPS == 1) %>% select(Year) %>% pull(1)
    sptoi_yrs <- yrs_mat %>% subset(Year %in% sel_yrs$Years & SPTOI == 1) %>% select(Year) %>% pull(1)
    gbps_yrs <- yrs_mat %>% subset(Year %in% sel_yrs$Years & GBPS == 1) %>% select(Year) %>% pull(1)
    resp_yrs <- yrs_mat %>% subset(Year %in% sel_yrs$Years & RESP == 1) %>% select(Year) %>% pull(1)
    
    # Generate List of Output
    seas_list <- list(
      "sbs_yrs" = sbs_yrs,
      "sas_yrs" = sas_yrs,
      "stoi_yrs" = stoi_yrs,
      "sms_yrs" = sms_yrs,
      "gbs_yrs" = gbs_yrs,
      "gso_yrs" = gso_yrs,
      "tbl_yrs" = tbl_yrs,
      "res_yrs" = res_yrs,
      "sbps_yrs" = sbps_yrs,
      "saps_yrs" = saps_yrs,
      "sptoi_yrs" = sptoi_yrs,
      "gbps_yrs" = gbps_yrs,
      "resp_yrs" = resp_yrs
    )
    

  # TEAMS -------------------------------------------------------------------

    # Generate List of Team Names & IDs
    tm_ref <- data.frame(
      Team_ID = c('ANA','ARI','ATF','ATL','BOS','BRO','BUF','CGY','CGS','CAR','CBH','CHI','CLE','COL','CLR','CBJ',
                  'DAL','DTC','DTF','DET','EDM','FLA','HAM','HAR','KCS','LAK','MDA','MNS','MIN','MTL','MTM','MTW',
                  'NSH','NJD','NYA','NYI','NYR','OAK','OTS','OTT','PHI','PHQ','PHX','PIT','PTP','QBC','QUE','SJS',
                  'STL','STE','TBL','TRA','TOR','TRS','VAN','VEG','WSH','WIN','WPG',
                  'TOT'
      ),
      Team_Name = c('Anaheim Ducks','Arizona Coyotes','Atlanta Flames','Atlanta Thrashers','Boston Bruins',
                    'Brooklyn Americans','Buffalo Sabres','Calgary Flames','California Golden Seals','Carolina Hurricanes',
                    'Chicago Black Hawks','Chicago Blackhawks','Cleveland Barons','Colorado Avalanche','Colorado Rockies',
                    'Columbus Blue Jackets','Dallas Stars','Detroit Cougars','Detroit Falcons','Detroit Red Wings',
                    'Edmonton Oilers','Florida Panthers','Hamilton Tigers','Hartford Whalers','Kansas City Scouts',
                    'Los Angeles Kings','Mighty Ducks of Anaheim','Minnesota North Stars','Minnesota Wild','Montreal Canadiens',
                    'Montreal Maroons','Montreal Wanderers','Nashville Predators','New Jersey Devils','New York Americans',
                    'New York Islanders','New York Rangers','Oakland Seals','Ottawa Senators','Ottawa Senators',
                    'Philadelphia Flyers','Philadelphia Quakers','Phoenix Coyotes','Pittsburgh Penguins','Pittsburgh Pirates',
                    'Quebec Athletic Club/Bulldogs','Quebec Nordiques','San Jose Sharks','St. Louis Blues','St. Louis Eagles',
                    'Tampa Bay Lightning','Toronto Arenas','Toronto Maple Leafs','Toronto St. Patricks','Vancouver Canucks',
                    'Vegas Golden Knights','Washington Capitals','Winnipeg Jets','Winnipeg Jets',
                    'Total'
      )
    )
    
    # Add Team Reference to Output List
    team_list <- list("tm_ref" = tm_ref)
    

  # RETURN OUTPUT -----------------------------------------------------------
  
    # Combine List Items
    setup_return <- append(seas_list, team_list)
    
    # Return
    return(setup_return)
  
} #end function
  
  