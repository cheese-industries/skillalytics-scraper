
########################################################################################################
#                                                                                                      #
#                               Skillalytics Scraper - Hockey-Reference                                #  
#                                                                                                      #  
########################################################################################################

#2021-06-14

#Package dependencies
# library(tidyverse)
# library(rvest)

href_dat <- function(seasons) {
  
  #---  SETUP  ---#
  
  #Avaiable years by table
  sbs_yrs <- c(1918:2004, 2006:2021)
  sas_yrs <- c(2008:2021)
  stoi_yrs <- c(2008:2021)
  sms_yrs <- c(1918:2004, 2006:2021)
  gbs_yrs <- c(1918:2004, 2006:2021)
  gso_yrs <- c(2009:2021)
  st_yrs <- c(1918:2004, 2006:2021)
  rss_yrs <- c(1918:2004, 2006:2021)
  
  #Create year availability matrix
  all_yrs <- c(1918:2021)
  yrs_mat <- data.frame(Year = all_yrs)
  yrs_mat <- yrs_mat %>%
    mutate(
      SBS = ifelse(Year %in% sbs_yrs, 1, 0),
      SAS = ifelse(Year %in% sas_yrs, 1, 0),
      STOI = ifelse(Year %in% stoi_yrs, 1, 0),
      SMS = ifelse(Year %in% sms_yrs, 1, 0),
      GBS = ifelse(Year %in% gbs_yrs, 1, 0),
      GSO = ifelse(Year %in% gso_yrs, 1, 0),
      ST = ifelse(Year %in% st_yrs, 1, 0),
      RSS = ifelse(Year %in% rss_yrs, 1, 0),
    )
  
  #Years selected
  sel_yrs <- data.frame(Years = seasons)
  
  #Years to scrape
  sbs_yrs <- yrs_mat %>% subset(Year %in% sel_yrs$Years & SBS == 1) %>% select(Year) %>% pull(1)
  sas_yrs <- yrs_mat %>% subset(Year %in% sel_yrs$Years & SAS == 1) %>% select(Year) %>% pull(1)
  stoi_yrs <- yrs_mat %>% subset(Year %in% sel_yrs$Years & STOI == 1) %>% select(Year) %>% pull(1)
  sms_yrs <- yrs_mat %>% subset(Year %in% sel_yrs$Years & SMS == 1) %>% select(Year) %>% pull(1)
  gbs_yrs <- yrs_mat %>% subset(Year %in% sel_yrs$Years & GBS == 1) %>% select(Year) %>% pull(1)
  gso_yrs <- yrs_mat %>% subset(Year %in% sel_yrs$Years & GSO == 1) %>% select(Year) %>% pull(1)
  st_yrs <- yrs_mat %>% subset(Year %in% sel_yrs$Years & ST == 1) %>% select(Year) %>% pull(1)
  rss_yrs <- yrs_mat %>% subset(Year %in% sel_yrs$Years & RSS == 1) %>% select(Year) %>% pull(1)
  
  
  #---  SKATER TABLES  ---#
  
  ## Skater Basic Stats
    
    #Create table url variables
    sbs_urlstrt <- 'https://www.hockey-reference.com/leagues/NHL_'
    sbs_urlend <- '_skaters.html'
    
    #Create xpath variables to get player id 
    sbs_xpath_strt <- '//*[@id="stats"]/tbody/tr['
    sbs_xpath_mid <- ']/td['
    sbs_xpath_end <- ']/a'
    
    #Create empty dataframe to fill
    sbs_df <- setNames(data.frame(matrix(ncol = 31, nrow = 0)),
                       c("Season", "Rk", "Player", "Age", "Tm",'Team_Bkdwn_Flag', 'Seas_Sum_Flag', "Pos",
                         "GP","G", "A", "PTS", "+/-", "PIM", "PS", "EV", "PP", "SH", "GW", "EV.1", "PP.1", "SH.1",
                         "S", "S%", "TOI", "ATOI", "BLK", "HIT", "FOW", "FOL", "FO%"))
    sbs_id_df <- data.frame()
    
    #Scrape data
    for (i in sbs_yrs){
      
      #Create url to scrape
      sbs_url <- read_html(paste(sbs_urlstrt, i, sbs_urlend, sep=""))
      
      #Get table
      sbs_df_temp <- sbs_url %>%
        html_node('table#stats') %>%    # select the desired table
        html_table()
      
      #Edit colnames
      names(sbs_df_temp) <- as.matrix(sbs_df_temp[1, ])
      sbs_df_temp <- sbs_df_temp[-1, ]
      sbs_df_temp <- cbind(Season = i, sbs_df_temp)
      
      #Get number of players in table for next loop
      rows <- (1:nrow(sbs_df_temp))
      
      #Loop through each row of table to get player url (to be used for player id)
      for (l in rows) {
        
        #Get url (player id)
        sbs_id_df_temp <- sbs_url %>%
          html_node(
            xpath = paste(sbs_xpath_strt, l, sbs_xpath_mid, 1, sbs_xpath_end, sep = "")
          ) %>%
          html_attr('href')
        
        #Turn into df to combine with stats table
        sbs_id_df_temp <- data.frame(sbs_id_df_temp)
        sbs_id_df <- rbind(sbs_id_df, sbs_id_df_temp)
        
      }
      
      #Combine data
      sbs_df_temp <- cbind(sbs_df_temp, sbs_id_df)
      
      #Find rows of players that have multiple listings based on playing for more than one team in season
      sbs_tots <- sbs_df_temp %>%
        select(
          Season, sbs_id_df_temp, Tm 
        ) %>%
        subset(
          Tm == 'TOT'
        )
      
      #Mark these based on keeping season totals and broken down by team rows for removal later
      sbs_df_temp <- sbs_df_temp %>%
        mutate(
          Match = sbs_df_temp$sbs_id_df_temp %in% sbs_tots$sbs_id_df_temp,
          Team_Bkdwn_Flag = ifelse(Match == TRUE & Tm != 'TOT', 1, 0),
          Seas_Sum_Flag = ifelse(Tm == 'TOT', 1, 0)
        ) %>%
        relocate(
          c(Team_Bkdwn_Flag, Seas_Sum_Flag), .after = Tm
        ) %>%
        select(
          -Match
        )
      
      #Remove blank rows
      sbs_df_temp <- sbs_df_temp[!(sbs_df_temp$Rk=="Rk"),]
      
      #Add to full dataframe  
      sbs_df <- rbind.fill(sbs_df, sbs_df_temp)
      
      #Clear temporary df's
      sbs_df_temp <- NULL
      sbs_id_df <- NULL
      
    }
    
    #Set colnames for table
    colnames(sbs_df)[1:32] <- c('Season', 'Rk', 'Player_Name', 'Age', 'Team', 'Team_Bkdwn_Flag', 'Seas_Sum_Flag', 'Pos1',
                                'GP', 'G', 'A', 'PTS', 'PlusMinus', 'PIM', 'PS', 'esG', 'ppG', 'shG',	'GWG', 'esA', 'ppA', 'shA',
                                'S', 'S_Pct', 'TOI', 'Avg_TOI', 'BLK', 'HIT', 'FOW', 'FOL',	'FO_Pct', 'Player_URL')
    
    #Edit table to convert player url into player id, relocate next to player name, and remove rank col
    sbs_df <- sbs_df %>%
      mutate(
        Player_ID = substr(substring(Player_URL, 12),1,nchar(substring(Player_URL, 12))-5)
      ) %>%
      relocate(
        Player_ID, .before = Player_Name
      ) %>%
      select(
        -Rk, -Player_URL
      ) %>%
      arrange(
        Season,
        Player_ID
      )

  
  ## Skater Advanced Stats
  
  #Check if table should be skipped based on years selected
  if (length(sas_yrs) != 0) {
    
    #Create table url variables
    sas_urlstrt <- 'https://www.hockey-reference.com/leagues/NHL_'
    sas_urlend <- '_skaters-advanced.html'
    
    #Create xpath variables to get player id
    sas_xpath_strt <- '//*[@id="stats_adv_rs"]/tbody/tr['
    sas_xpath_mid <- ']/td['
    sas_xpath_end <- ']/a'
    
    #Create empty dataframe to fill
    sas_df <- setNames(data.frame(matrix(ncol = 27, nrow = 0)),
                       c("Season", "Rk", "Player", "Age", "Tm", "Pos", "GP", "CF", "CA", "CF%", "CF% rel", "FF", "FA", "FF%",
                         "FF% rel", "oiSH%", "oiSV%", "PDO", "oZS%", "dZS%", "TOI/60", "TOI(EV)", "TK", "GV", "E+/-", "SAtt.", "Thru%"))
    sas_id_df <- data.frame()
    
    #Scrape data
    for (i in sas_yrs){
      
      #Create url to scrape
      sas_url <- read_html(paste(sas_urlstrt, i, sas_urlend, sep=""))
      
      #Get table
      sas_df_temp <- sas_url %>%
        html_node('table#stats_adv_rs') %>%    # select the desired table
        html_table()
      
      #Edit colnames
      names(sas_df_temp) <- as.matrix(sas_df_temp[1, ])
      sas_df_temp <- sas_df_temp[-1, ]
      sas_df_temp <- cbind(Season = i, sas_df_temp)
      
      #Get number of players in table for next loop
      rows <- (1:nrow(sas_df_temp))
      
      #Loop through each row of table to get player url (to be used for player id)
      for (l in rows) {
        
        #Get url (player id)
        sas_id_df_temp <- sas_url %>%
          html_node(
            xpath = paste(sas_xpath_strt, l, sas_xpath_mid, 1, sas_xpath_end, sep = "")
          ) %>%
          html_attr('href')
        
        #Turn into df to combine with stats table
        sas_id_df_temp <- data.frame(sas_id_df_temp)
        sas_id_df <- rbind(sas_id_df, sas_id_df_temp)
        
      }
      
      #Combine data
      sas_df_temp <- cbind(sas_df_temp, sas_id_df)
      
      #Remove blank rows
      sas_df_temp <- sas_df_temp[!(sas_df_temp$Rk=="Rk"),]
      
      #Add to full dataframe 
      sas_df <- rbind.fill(sas_df, sas_df_temp)
      
      #Clear temporary df's
      sas_df_temp <- NULL
      sas_id_df <- NULL
      
    }
    
    #Set colnames for table
    colnames(sas_df)[1:28] <- c('Season', 'Rk', 'Player_Name', 'Age', 'Team', 'Pos', 'GP',
                                'esCF', 'esCA', 'esCF_Pct', 'esRelCF_Pct', 'esFF', 'esFA', 'esFF_Pct', 'esRelFF_Pct',
                                'oiSH_Pct', 'oiSV_Pct', 'PDO', 'oZS_Pct', 'dZS_Pct', 'TOI_per60', 'esTOI_per60', 'TK', 'GV',
                                'xPlusMinus','alSAtt','SAThruToNet_Pct', 'Player_URL')
    
    #Breakout Pos col in two based on players who played multiple positions (denoted by "/")
    sas_df <- separate(data = sas_df, col = Pos, into = c("Pos1", "Pos2"), sep = "/")
    
    #Edit table to convert player url into player id, relocate next to player name, and remove rank col
    sas_df <- sas_df %>%
      mutate(
        Player_ID = substr(substring(Player_URL, 12),1,nchar(substring(Player_URL, 12))-5)
      ) %>%
      relocate(
        Player_ID, .before = Player_Name
      ) %>%
      select(
        -Rk, -Player_URL
      ) %>%
      arrange(
        Season,
        Player_ID
      )
    
  } else {
    #skip
  }
  
  
  ## Skater TOI Stats
  
  #Check if table should be skipped based on years selected
  if (length(stoi_yrs) != 0) {
    
    #Create table url variables
    stoi_urlstrt <- 'https://www.hockey-reference.com/leagues/NHL_'
    stoi_urlend <- '_skaters-time-on-ice.html'
    
    #Create xpath variables to get player id
    stoi_xpath_strt <- '//*[@id="stats_toi"]/tbody/tr['
    stoi_xpath_mid <- ']/td['
    stoi_xpath_end <- ']/a'
    
    #Create empty dataframe to fill
    stoi_df <- setNames(data.frame(matrix(ncol = 19, nrow = 0)),
                        c("Season", "Rk", "Player", "Tm", "Pos", "Shift", "GP", "TOI", "CF% Rel", "GF/60", "GA/60",
                          "TOI.1", "CF% Rel.1", "GF/60.1", "GA/60.1", "TOI.2", "CF% Rel.2", "GF/60.2", "GA/60.2"))
    stoi_id_df <- data.frame()
    
    #Scrape data
    for (i in stoi_yrs){
      
      #Create url to scrape
      stoi_url <- read_html(paste(stoi_urlstrt, i, stoi_urlend, sep=""))
      
      #Get table
      stoi_df_temp <- stoi_url %>%
        html_node('table#stats_toi') %>%    # select the desired table
        html_table()
      
      #Edit colnames
      names(stoi_df_temp) <- as.matrix(stoi_df_temp[1, ])
      stoi_df_temp <- stoi_df_temp[-1, ]
      stoi_df_temp <- cbind(Season = i, stoi_df_temp)
      
      #Get number of players in table for next loop
      rows <- (1:nrow(stoi_df_temp))
      
      #Loop through each row of table to get player url (to be used for player id)
      for (l in rows) {
        
        #Get url (player id)
        stoi_id_df_temp <- stoi_url %>%
          html_node(
            xpath = paste(stoi_xpath_strt, l, stoi_xpath_mid, 1, stoi_xpath_end, sep = "")
          ) %>%
          html_attr('href')
        
        #Turn into df to combine with stats table
        stoi_id_df_temp <- data.frame(stoi_id_df_temp)
        stoi_id_df <- rbind(stoi_id_df, stoi_id_df_temp)
        
      }
      
      #Combine data
      stoi_df_temp <- cbind(stoi_df_temp, stoi_id_df)
      
      #Remove blank rows
      stoi_df_temp <- stoi_df_temp[!(stoi_df_temp$Rk=="Rk"),]
      
      #Remove null cols
      stoi_df_temp <- stoi_df_temp[-c(8, 13, 18)]
      
      #Add to full dataframe 
      stoi_df <- rbind.fill(stoi_df, stoi_df_temp)
      
      #Clear temporary df's
      stoi_df_temp <- NULL
      stoi_id_df <- NULL
      
    }
    
    #Set colnames for table
    colnames(stoi_df)[1:20] <- c('Season', 'Rk', 'Player_Name', 'Team', 'Pos', 'Avg_ShiftLength_perGm', 'GP',
                                 'esTOI_perGm', 'esRelCF_Pct', 'esGF_per60', 'esGA_per60', 'ppTOI_perGm', 'ppRelCF_Pct',
                                 'ppGF_per60', 'ppGA_per60', 'shTOI_perGm', 'shRelCF_Pct', 'shGF_per60', 'shGA_per60', 'Player_URL')
    
    #Breakout Pos col in two based on players who played multiple positions (denoted by "/")
    stoi_df <- separate(data = stoi_df, col = Pos, into = c("Pos1", "Pos2"), sep = "/")
    
    #Edit table to convert player url into player id, relocate next to player name, and remove rank col
    stoi_df <- stoi_df %>%
      mutate(
        Player_ID = substr(substring(Player_URL, 12),1,nchar(substring(Player_URL, 12))-5)
      ) %>%
      relocate(
        Player_ID, .before = Player_Name
      ) %>%
      select(
        -Rk, -Player_URL
      ) %>%
      arrange(
        Season,
        Player_ID
      )
    
  } else {
    #skip
  }
  
  
  ## Skater Misc Stats
  
    #Create table url variables
    sms_urlstrt <- 'https://www.hockey-reference.com/leagues/NHL_'
    sms_urlend <- '_skaters-misc.html'
    
    #Create xpath variables to get player id
    sms_xpath_strt <- '//*[@id="stats_misc"]/tbody/tr['
    sms_xpath_mid <- ']/td['
    sms_xpath_end <- ']/a'
    
    #Create empty dataframe to fill
    sms_df <- data.frame("Season" = integer(),
                         "Rk" = character(),
                         "Player" = character(),
                         "Age" = character(),
                         "Tm" = character(),
                         "Pos" = character(),
                         "GP" = character(),
                         "GC...8" = character(),
                         "G...9" = character(),
                         "A...10" = character(),
                         "PTS...11" = character(),
                         "GC...12" = character(),
                         "PIM" = character(),
                         "S" = character(),
                         "G...15" = character(),
                         "A...16" = character(),
                         "PTS...17" = character(),
                         "GC...18" = character(),
                         "TGF" = character(),
                         "PGF" = character(),
                         "TGA" = character(),
                         "PGA" = character(),
                         "+/-" = character(),
                         "xGF" = character(),
                         "xGA" = character(),
                         "E+/-" = character(),
                         "OPS" = character(),
                         "DPS" = character(),
                         "PS" = character(),
                         "Att." = character(),
                         "Made" = character(),
                         "Miss" = character(),
                         "Pct." = character(),
                         check.names=FALSE
                         )
    sms_id_df <- data.frame()
    
    #Scrape data
    for (i in sms_yrs){
      
      #Create url to scrape
      sms_url <- read_html(paste(sms_urlstrt, i, sms_urlend, sep=""))
      
      #Get table
      sms_df_temp <- sms_url %>%
        html_node('table#stats_misc') %>%    # select the desired table
        html_table()
      
      #Edit colnames
      names(sms_df_temp) <- as.matrix(sms_df_temp[1, ])
      sms_df_temp <- sms_df_temp[-1, ]
      sms_df_temp <- cbind(Season = i, sms_df_temp)
      
      #Get number of players in table for next loop
      rows <- (1:nrow(sms_df_temp))
      
      #Loop through each row of table to get player url (to be used for player id)
      for (l in rows) {
        
        #Get url (player id)
        sms_id_df_temp <- sms_url %>%
          html_node(
            xpath = paste(sms_xpath_strt, l, sms_xpath_mid, 1, sms_xpath_end, sep = "")
          ) %>%
          html_attr('href')
        
        #Turn into df to combine with stats table
        sms_id_df_temp <- data.frame(sms_id_df_temp)
        sms_id_df <- rbind(sms_id_df, sms_id_df_temp)
        
      }
      
      #Combine data
      sms_df_temp <- cbind(sms_df_temp, sms_id_df)
      
      #Remove blank rows
      sms_df_temp <- sms_df_temp[!(sms_df_temp$Rk=="Rk"),]
      
      #Add to full dataframe 
      sms_df <- bind_rows(sms_df, sms_df_temp)
      
      #Clear temporary df's
      sms_df_temp <- NULL
      sms_id_df <- NULL
      
    }
    
    #Set colnames for table
    colnames(sms_df)[1:34] <- c('Season', 'Rk', 'Player_Name', 'Age', 'Team', 'Pos', 'GP', 'GC', 'G_perGm', 'A_perGm', 'PTS_perGm', 'GC_perGm',
                                'PIM_perGm', 'S_perGm', 'adjG', 'adjA', 'adjPTS', 'adjGC', 'oiTGF', 'oiPPGF', 'oiTGA', 'oiPKGA', 'PlusMinus',
                                'xGF', 'xGA', 'xPlusMinus', 'OPS', 'DPS', 'PS', 'soAtt', 'soMade', 'soMiss', 'soPct', 'Player_URL')
    
    #Breakout Pos col in two based on players who played multiple positions (denoted by "/")
    sms_df <- separate(data = sms_df, col = Pos, into = c("Pos1", "Pos2"), sep = "/")
    
    #Edit table to convert player url into player id, relocate next to player name, and remove rank col
    sms_df <- sms_df %>%
      mutate(
        Player_ID = substr(substring(Player_URL, 12),1,nchar(substring(Player_URL, 12))-5)
      ) %>%
      relocate(
        Player_ID, .before = Player_Name
      ) %>%
      select(
        -Rk, -Player_URL
      ) %>%
      arrange(
        Season,
        Player_ID
      )
    
  
  ## Combine Basic, Advanced, TOI, and Advanced tables
    
    skater_season_stats <- sbs_df
    
    if (length(sas_yrs) != 0) {
      
      #Add advanced table to basic stats
      skater_season_stats <- skater_season_stats %>%
        left_join(
          sas_df[, c('Season', 'Player_ID', 'Team', 'Pos2', 'esCF', 'esCA', 'esCF_Pct', 'esRelCF_Pct', 'esFF', 'esFA',
                     'esFF_Pct', 'esRelFF_Pct', 'oiSH_Pct', 'oiSV_Pct', 'PDO', 'oZS_Pct', 'dZS_Pct', 'TOI_per60',
                     'esTOI_per60', 'TK', 'GV', 'xPlusMinus','alSAtt','SAThruToNet_Pct')],
          by = c('Season', 'Player_ID', 'Team')
        ) %>%
        relocate(
          Pos2, .after = Pos1
        )
    } else {
      #skip
    }
    
    if (length(stoi_yrs) != 0) {
      
      #Add TOI table
      skater_season_stats <- skater_season_stats %>%
        left_join(
          stoi_df[, c('Season', 'Player_ID', 'Team', 'Avg_ShiftLength_perGm', 'esTOI_perGm', 'esGF_per60', 'esGA_per60', 'ppTOI_perGm',
                      'ppRelCF_Pct', 'ppGF_per60', 'ppGA_per60', 'shTOI_perGm', 'shRelCF_Pct', 'shGF_per60', 'shGA_per60')],
          by = c('Season', 'Player_ID', 'Team')
        )
      
    } else {
      #skip
    }
    
    #Add Misc table
    skater_season_stats <- skater_season_stats %>%
      left_join(
        sms_df[, c('Season', 'Player_ID', 'Team', 'GC', 'G_perGm', 'A_perGm', 'PTS_perGm', 'GC_perGm', 'PIM_perGm',
                   'S_perGm', 'adjG', 'adjA', 'adjPTS', 'adjGC', 'oiTGF', 'oiPPGF', 'oiTGA', 'oiPKGA', 'xGF', 'xGA',
                   'OPS', 'DPS', 'soAtt', 'soMade', 'soMiss', 'soPct')],
        by = c('Season', 'Player_ID', 'Team')
      )
  
  
  #-----------------------#
  
  
  #---  GOALIE TABLES  ---#
  
  ## Goalie Basic Stats
  
    #Create table url variables
    gbs_urlstrt <- 'https://www.hockey-reference.com/leagues/NHL_'
    gbs_urlend <- '_goalies.html'
    
    #Create xpath variables to get player id
    gbs_xpath_strt <- '//*[@id="stats"]/tbody/tr['
    gbs_xpath_mid <- ']/td['
    gbs_xpath_end <- ']/a'
    
    #Create empty dataframe to fill
    gbs_df <- setNames(data.frame(matrix(ncol = 27, nrow = 0)),
                       c("Season", "Rk", "Player", "Age", "Tm", "GP", "GS", "W", "L", "T/O",
                         "GA", "SA", "SV", "SV%", "GAA", "SO", "GPS", "MIN", "QS", "QS%",
                         "RBS", "GA%-", "GSAA", "G", "A", "PTS", "PIM"))
    gbs_id_df <- data.frame()
    
    #Scrape data
    for (i in gbs_yrs){
      
      #Create url to scrape
      gbs_url <- read_html(paste(gbs_urlstrt, i, gbs_urlend, sep=""))
      
      #Get table
      gbs_df_temp <- gbs_url %>%
        html_node('table#stats') %>%    # select the desired table
        html_table()
      
      #Edit colnames
      names(gbs_df_temp) <- as.matrix(gbs_df_temp[1, ])
      gbs_df_temp <- gbs_df_temp[-1, ]
      gbs_df_temp <- cbind(Season = i, gbs_df_temp)
      
      #Get number of goalies in table for next loop
      rows <- (1:nrow(gbs_df_temp))
      
      #Loop through each row of table to get player url (to be used for player id)
      for (l in rows) {
        
        #Get url (player id)
        gbs_id_df_temp <- gbs_url %>%
          html_node(
            xpath = paste(gbs_xpath_strt, l, gbs_xpath_mid, 1, gbs_xpath_end, sep = "")
          ) %>%
          html_attr('href')
        
        #Turn into df to combine with stats table
        gbs_id_df_temp <- data.frame(gbs_id_df_temp)
        gbs_id_df <- rbind(gbs_id_df, gbs_id_df_temp)
        
      }
      
      #Combine data
      gbs_df_temp <- cbind(gbs_df_temp, gbs_id_df)
      
      #Find rows of goalies that have multiple listings based on playing for more than one team in season
      gbs_tots <- gbs_df_temp %>%
        select(
          Season, gbs_id_df_temp, Tm 
        ) %>%
        subset(
          Tm == 'TOT'
        )
      
      #Mark these based on keeping season totals and broken down by team rows for removal later
      gbs_df_temp <- gbs_df_temp %>%
        mutate(
          Match = gbs_df_temp$gbs_id_df_temp %in% gbs_tots$gbs_id_df_temp,
          Tm_Bkdwn_Flag = ifelse(Match == TRUE & Tm != 'TOT', 1, 0),
          Seas_Sum_Flag = ifelse(Tm == 'TOT', 1, 0)
        ) %>%
        relocate(
          c(Tm_Bkdwn_Flag, Seas_Sum_Flag), .after = Tm
        ) %>%
        select(
          -Match
        )
      
      #Remove blank rows
      gbs_df_temp <- gbs_df_temp[!(gbs_df_temp$Rk=="Rk"),]
      
      #Add to full dataframe 
      gbs_df <- rbind(gbs_df, gbs_df_temp)
      
      #Clear temporary df's
      gbs_df_temp <- NULL
      gbs_id_df <- NULL
      
    }
    
    #Set colnames for table
    colnames(gbs_df)[1:30] <- c('Season', 'Rk', 'Player_Name', 'Age', 'Team',  'Team_Bkdwn_Flag', 'Seas_Sum_Flag', 'GP', 'GS', 'W', 'L',
                                'TOSL', 'GA', 'SA', 'SV','SV_Pct', 'GAA', 'SO', 'GPS', 'MIN', 'QS', 'QS_Pct', 'RBS', 'GA_Pct_Rel', 'GSAA',
                                'G', 'A', 'PTS', 'PIM', 'Player_URL')
    
    #Add Pos 'G'
    gbs_df <- gbs_df %>%
      mutate(
        Pos = 'G'
      ) %>%
      relocate(
        Pos, .before = GP
      )
    
    #Edit table to convert player url into player id, relocate next to player name, and remove rank col
    gbs_df <- gbs_df %>%
      mutate(
        Player_ID = substr(substring(Player_URL, 12),1,nchar(substring(Player_URL, 12))-5)
      ) %>%
      relocate(
        Player_ID, .before = Player_Name
      ) %>%
      select(
        -Rk, -Player_URL
      )
  
    goalie_season_stats <- gbs_df
  
  ## Goalie Shootout Stats
  
  #Check if table should be skipped based on years selected
  if (length(gso_yrs) != 0) {
    
    #Create table url variables
    gso_urlstrt <- 'https://www.hockey-reference.com/leagues/NHL_'
    gso_urlend <- '_skaters-shootout.html'
    
    #Create xpath variables to get player id
    gso_xpath_strt <- '//*[@id="shootout_goalies"]/tbody/tr['
    gso_xpath_end <- ']/th/a'
    
    #Create empty dataframe to fill
    gso_df <- setNames(data.frame(matrix(ncol = 7, nrow = 0)),
                       c("Season", "Player", "Tm", "Att.", "Made", "Miss", "Pct."))
    gso_id_df <- data.frame()
    
    #Scrape data
    for (i in gso_yrs){
      
      #Create url to scrape
      gso_url <- read_html(paste(gso_urlstrt, i, gso_urlend, sep=""))
      
      #Get table
      gso_df_temp <- gso_url %>% html_nodes(xpath = '//comment()') %>%    # select comment nodes
        html_text() %>%    # extract comment text
        paste(collapse = '') %>%    # collapse to a single string
        read_html() %>%    # reparse to HTML
        html_node('table#shootout_goalies') %>%    # select the desired table
        html_table() %>%    # parse table
        .[colSums(is.na(.)) < nrow(.)]    # get rid of spacer columns
      gso_df_temp <- cbind(Season = i, gso_df_temp)
      
      #Get number of players in table for next loop
      rows <- (1:nrow(gso_df_temp))
      
      #Loop through each row of table to get player url (to be used for player id)
      for (l in rows) {
        
        #Get url (player id)
        gso_id_df_temp <- gso_url %>% html_nodes(xpath = '//comment()') %>%    # select comment nodes
          html_text() %>%    # extract comment text
          paste(collapse = '') %>%    # collapse to a single string
          read_html() %>%    # reparse to HTML
          html_node('table#shootout_goalies') %>%    # select the desired table
          html_node(
            xpath = paste(gso_xpath_strt, l, gso_xpath_end, sep = "")
          ) %>%
          html_attr('href')
        
        #Turn into df to combine with stats table
        gso_id_df_temp <- data.frame(gso_id_df_temp)
        gso_id_df <- rbind(gso_id_df, gso_id_df_temp)
        
      }
      
      #Combine data
      gso_df_temp <- cbind(gso_df_temp, gso_id_df)
      
      #Add to full dataframe 
      gso_df <- rbind(gso_df, gso_df_temp)
      
      #Clear temporary df's
      gso_df_temp <- NULL
      gso_id_df <- NULL
      
    }
    
    #Set colnames for table
    colnames(gso_df)[1:8] <- c('Season', 'Player_Name', 'Team', 'soAtt', 'soMade', 'soMiss', 'soPct', 'Player_URL')
    
    #Edit table to convert player url into player id, relocate next to player name, and remove rank col
    gso_df <- gso_df %>%
      mutate(
        Player_ID = substr(substring(Player_URL, 12),1,nchar(substring(Player_URL, 12))-5)
      ) %>%
      relocate(
        Player_ID, .before = Player_Name
      ) %>%
      select(
        -Player_URL
      )
    
    
    ## Combine Basic and Shootout Stats tables together
    
    #Add shoout table to basic stats
    goalie_season_stats <- goalie_season_stats %>%
      left_join(
        gso_df[, c('Season', 'Player_ID', 'Team', 'soAtt', 'soMade', 'soMiss', 'soPct')],
        by = c('Season', 'Player_ID', 'Team')
      )
  
  } else{
    #skip
  }
  
    
  #-----------------------#
  
  
  #---  STANDING TABLES  ---#
  
  ## League Standings
  
    #Create table url variables
    st_urlstrt <- 'https://www.hockey-reference.com/leagues/NHL_'
    st_urlend <- '_standings.html'
    
    # no conf from 1918-1974 (#standings)
    # clarence campbell and prince of wales conf from 1975-1993 (#standings_CAM; #standings_WAL)
    # east and west conf from 1994+ (#standings_EAS; #standings_WES)
    
    #Create empty dataframe to fill
    st_df <- setNames(data.frame(matrix(ncol = 21, nrow = 0)),
                      c("Season", "Half", "Conference", "Division", "Team", "GP", "W", "L", "OL", "T",
                        "PTS", "PTS%", "GF", "GA", "SRS", "SOS", "RPt%", "ROW", "RW", "RgRec", "RgPt%"))
    st_df$Conference <- as.character(st_df$Conference)
    
    #Scrape data
    for (i in st_yrs){
      
      #Create url to scrape
      st_url <- read_html(paste(st_urlstrt, i, st_urlend, sep=""))
      
      #If stmt to deterine how to scrape & build table based on season
      #Pre 1922 had First and Second half seasons
      if (i <= 1921) {
        
        #Get table
        st_df_temp <- st_url %>%
          html_node('table#standings') %>%
          html_table()
        st_df_temp <- cbind(Season = i, st_df_temp)
        
        #Add col for season Half and rename Team col
        st_df_temp <- st_df_temp %>%
          mutate(
            rownum = as.numeric(row.names(st_df_temp)),
            Half = ifelse(rownum < which(grepl('2nd Half', get('1st Half'))), 'First', 'Second')
          ) %>%
          select(
            -rownum
          ) %>%
          dplyr::rename(
            Team = '1st Half'
          )
        
        #Remove blank rows
        st_df_temp <- st_df_temp[!(st_df_temp$Team == '2nd Half'),]
        
        #1922 to 1926 no longer First and Second half seasons, no divisions
      } else if (i > 1921 & i <= 1926) {
        
        #Get table
        st_df_temp <- st_url %>%
          html_node('table#standings') %>%
          html_table()
        
        #Edit colnames
        st_df_temp <- cbind(Season = i, st_df_temp)
        colnames(st_df_temp)[2] <- 'Team'
        
        #1927 to 1938 divisions added  
      } else if (i > 1926 & i <= 1938) {
        
        #Get table
        st_df_temp <- st_url %>%
          html_node('table#standings') %>%
          html_table()
        
        #Edit colnames
        st_df_temp <- cbind(Season = i, st_df_temp)
        colnames(st_df_temp)[2] <- 'Team'
        
        #Add col for division
        st_df_temp <- st_df_temp %>%
          mutate(
            rownum = as.numeric(row.names(st_df_temp)),
            Division = gsub(" Division", "", 
                            ifelse(rownum <= which(grepl('* Division', Team[2:nrow(st_df_temp)])), Team[1],
                                   Team[which(grepl('* Division', Team[2:nrow(st_df_temp)]))+1])
            )
          ) %>%
          select(
            -rownum
          ) %>%
          relocate(
            Division, .before = Team
          )
        
        #Remove blank rows
        st_df_temp <- st_df_temp[!grepl('* Division', st_df_temp$Team),]
        
        #1939 to 1967 divisions removed
      } else if (i > 1938 & i <= 1967) {
        
        #Get table
        st_df_temp <- st_url %>%
          html_node('table#standings') %>%
          html_table()
        
        #Edit colnames
        st_df_temp <- cbind(Season = i, st_df_temp)
        colnames(st_df_temp)[2] <- 'Team'
        
        #1967 to 1974 split into East and West divisions    
      } else if (i > 1967 & i <= 1974) {
        
        #Get table
        st_df_temp <- st_url %>%
          html_node('table#standings') %>%
          html_table()
        
        #Edit colnames
        st_df_temp <- cbind(Season = i, st_df_temp)
        colnames(st_df_temp)[2] <- 'Team'
        
        #Add col for division
        st_df_temp <- st_df_temp %>%
          mutate(
            rownum = as.numeric(row.names(st_df_temp)),
            Division = gsub(" Division", "", 
                            ifelse(rownum <= which(grepl('* Division', Team[2:nrow(st_df_temp)])), Team[1],
                                   Team[which(grepl('* Division', Team[2:nrow(st_df_temp)]))+1])
            )
          ) %>%
          select(
            -rownum
          ) %>%
          relocate(
            Division, .before = Team
          )
        
        #Remove blank rows
        st_df_temp <- st_df_temp[!grepl('* Division', st_df_temp$Team),]
        
        #1974 to 1993 split into Clarence Campbell and Prince of Wales conferences 
      } else if (i > 1974 & i <= 1993) {
        
        #Get Clarence Campbell table
        st_df_cam_temp <- st_url %>%
          html_node('table#standings_CAM') %>%
          html_table()
        
        #Edit cols
        st_df_cam_temp <- cbind(Season = i, Conference = as.character('Clarence Campbell'), st_df_cam_temp)
        colnames(st_df_cam_temp)[3] <- 'Team'
        
        #Add col for division
        st_df_cam_temp <- st_df_cam_temp %>%
          mutate(
            rownum = as.numeric(row.names(st_df_cam_temp)),
            Division = gsub(" Division", "", 
                            ifelse(rownum <= which(grepl('* Division', Team[2:nrow(st_df_cam_temp)])), Team[1],
                                   Team[which(grepl('* Division', Team[2:nrow(st_df_cam_temp)]))+1])
            )
          ) %>%
          select(
            -rownum
          ) %>%
          relocate(
            Division, .before = Team
          )
        
        #Remove blank rows
        st_df_cam_temp <- st_df_cam_temp[!grepl('* Division', st_df_cam_temp$Team),]
        
        #Get Prince of Wales table
        st_df_wal_temp <- st_url %>%
          html_node('table#standings_WAL') %>%
          html_table()
        
        #Edit cols
        st_df_wal_temp <- cbind(Season = i, Conference = as.character('Prince of Wales'), st_df_wal_temp)
        colnames(st_df_wal_temp)[3] <- 'Team'
        
        #Add col for division
        st_df_wal_temp <- st_df_wal_temp %>%
          mutate(
            rownum = as.numeric(row.names(st_df_wal_temp)),
            Division = gsub(" Division", "", 
                            ifelse(rownum <= which(grepl('* Division', Team[2:nrow(st_df_wal_temp)])), Team[1],
                                   Team[which(grepl('* Division', Team[2:nrow(st_df_wal_temp)]))+1])
            )
          ) %>%
          select(
            -rownum
          ) %>%
          relocate(
            Division, .before = Team
          )
        
        #Remove blank rows
        st_df_wal_temp <- st_df_wal_temp[!grepl('* Division', st_df_wal_temp$Team),]
        
        #Combine conference tables
        st_df_temp <- rbind(st_df_cam_temp, st_df_wal_temp)
        
        #1994 conference names changed to Eastern and Western conferences
      } else if (i > 1993 & i <= 1998) {
        
        #Get Eastern table
        st_df_eas_temp <- st_url %>%
          html_node('table#standings_EAS') %>%
          html_table()
        
        #Edit cols
        st_df_eas_temp <- cbind(Season = i, Conference = as.character('Eastern'), st_df_eas_temp)
        colnames(st_df_eas_temp)[3] <- 'Team'
        
        #Add col for division
        st_df_eas_temp <- st_df_eas_temp %>%
          mutate(
            rownum = as.numeric(row.names(st_df_eas_temp)),
            Division = gsub(" Division", "", 
                            ifelse(rownum <= which(grepl('* Division', Team[2:nrow(st_df_eas_temp)])), Team[1],
                                   Team[which(grepl('* Division', Team[2:nrow(st_df_eas_temp)]))+1])
            )
          ) %>%
          select(
            -rownum
          ) %>%
          relocate(
            Division, .before = Team
          )
        
        #Remove blank rows
        st_df_eas_temp <- st_df_eas_temp[!grepl('* Division', st_df_eas_temp$Team),]
        
        #Get Western table
        st_df_wes_temp <- st_url %>%
          html_node('table#standings_WES') %>%
          html_table()
        
        #Edit cols
        st_df_wes_temp <- cbind(Season = i, Conference = as.character('Western'), st_df_wes_temp)
        colnames(st_df_wes_temp)[3] <- 'Team'
        
        #Add col for division
        st_df_wes_temp <- st_df_wes_temp %>%
          mutate(
            rownum = as.numeric(row.names(st_df_wes_temp)),
            Division = gsub(" Division", "", 
                            ifelse(rownum <= which(grepl('* Division', Team[2:nrow(st_df_wes_temp)])), Team[1],
                                   Team[which(grepl('* Division', Team[2:nrow(st_df_wes_temp)]))+1])
            )
          ) %>%
          select(
            -rownum
          ) %>%
          relocate(
            Division, .before = Team
          )
        
        #Remove blank rows
        st_df_wes_temp <- st_df_wes_temp[!grepl('* Division', st_df_wes_temp$Team),]
        
        #Combine conference tables
        st_df_temp <- rbind(st_df_eas_temp, st_df_wes_temp)
        
        #1994 to 2013 conference names changed to Eastern and Western conferences with 3 divisions each
      } else if (i > 1998 & i <= 2013) {
        
        #Get Eastern table
        st_df_eas_temp <- st_url %>%
          html_node('table#standings_EAS') %>%
          html_table()
        
        #Edit cols
        st_df_eas_temp <- cbind(Season = i, Conference = 'Eastern', st_df_eas_temp)
        colnames(st_df_eas_temp)[3] <- 'Team'
        
        #Add col for division
        st_df_eas_temp <- st_df_eas_temp %>%
          mutate(
            rownum = as.numeric(row.names(st_df_eas_temp)),
            Division = gsub(" Division", "", 
                            ifelse(rownum < which(grepl('* Division', Team))[2], Team[1],
                                   ifelse(rownum < which(grepl('* Division', Team))[3], Team[which(grepl('* Division', Team))[2]],
                                          Team[which(grepl('* Division', Team))[3]]))
            )
          ) %>%
          select(
            -rownum
          ) %>%
          relocate(
            Division, .before = Team
          )
        
        #Remove blank rows
        st_df_eas_temp <- st_df_eas_temp[!grepl('* Division', st_df_eas_temp$Team),]
        
        #Get Western table
        st_df_wes_temp <- st_url %>%
          html_node('table#standings_WES') %>%
          html_table()
        
        #Edit cols
        st_df_wes_temp <- cbind(Season = i, Conference = 'Western', st_df_wes_temp)
        colnames(st_df_wes_temp)[3] <- 'Team'
        
        #Add col for division
        st_df_wes_temp <- st_df_wes_temp %>%
          mutate(
            rownum = as.numeric(row.names(st_df_wes_temp)),
            Division = gsub(" Division", "", 
                            ifelse(rownum < which(grepl('* Division', Team))[2], Team[1],
                                   ifelse(rownum < which(grepl('* Division', Team))[3], Team[which(grepl('* Division', Team))[2]],
                                          Team[which(grepl('* Division', Team))[3]]))
            )
          ) %>%
          select(
            -rownum
          ) %>%
          relocate(
            Division, .before = Team
          )
        
        #Remove blank rows
        st_df_wes_temp <- st_df_wes_temp[!grepl('* Division', st_df_wes_temp$Team),]
        
        #Combine conference tables
        st_df_temp <- rbind(st_df_eas_temp, st_df_wes_temp)
        
        #2013 to 2020 changed to 2 divisions in each conference
      } else if (i > 2013 & i <= 2020) {
        
        #Get Eastern table
        st_df_eas_temp <- st_url %>%
          html_node('table#standings_EAS') %>%
          html_table()
        
        #Edit cols
        st_df_eas_temp <- cbind(Season = i, Conference = as.character('Eastern'), st_df_eas_temp)
        colnames(st_df_eas_temp)[3] <- 'Team'
        
        #Add col for division
        st_df_eas_temp <- st_df_eas_temp %>%
          mutate(
            rownum = as.numeric(row.names(st_df_eas_temp)),
            Division = gsub(" Division", "", 
                            ifelse(rownum <= which(grepl('* Division', Team[2:nrow(st_df_eas_temp)])), Team[1],
                                   Team[which(grepl('* Division', Team[2:nrow(st_df_eas_temp)]))+1])
            )
          ) %>%
          select(
            -rownum
          ) %>%
          relocate(
            Division, .before = Team
          )
        
        #Remove blank rows
        st_df_eas_temp <- st_df_eas_temp[!grepl('* Division', st_df_eas_temp$Team),]
        
        #Get Western table
        st_df_wes_temp <- st_url %>%
          html_node('table#standings_WES') %>%
          html_table()
        
        #Edit cols
        st_df_wes_temp <- cbind(Season = i, Conference = as.character('Western'), st_df_wes_temp)
        colnames(st_df_wes_temp)[3] <- 'Team'
        
        #Add col for division
        st_df_wes_temp <- st_df_wes_temp %>%
          mutate(
            rownum = as.numeric(row.names(st_df_wes_temp)),
            Division = gsub(" Division", "", 
                            ifelse(rownum <= which(grepl('* Division', Team[2:nrow(st_df_wes_temp)])), Team[1],
                                   Team[which(grepl('* Division', Team[2:nrow(st_df_wes_temp)]))+1])
            )
          ) %>%
          select(
            -rownum
          ) %>%
          relocate(
            Division, .before = Team
          )
        
        #Remove blank rows
        st_df_wes_temp <- st_df_wes_temp[!grepl('* Division', st_df_wes_temp$Team),]
        
        #Combine conference tables
        st_df_temp <- rbind(st_df_eas_temp, st_df_wes_temp)
        
        #2021 no conferences, four divisions      
      } else if (i > 2020) {
        
        #Get table
        st_df_temp <- st_url %>%
          html_node('table#standings') %>%
          html_table()
        
        #Edit cols
        st_df_temp <- cbind(Season = i, st_df_temp)
        colnames(st_df_temp)[2] <- 'Team'
        
        #Add col for division
        st_df_temp <- st_df_temp %>%
          mutate(
            rownum = as.numeric(row.names(st_df_temp)),
            Division = gsub(" Division", "", 
                            ifelse(rownum < which(grepl('* Division', Team))[2], Team[1],
                                   ifelse(rownum < which(grepl('* Division', Team))[3], Team[which(grepl('* Division', Team))[2]],
                                          ifelse(rownum < which(grepl('* Division', Team))[4], Team[which(grepl('* Division', Team))[3]],
                                                 Team[which(grepl('* Division', Team))[4]])))
            )
          ) %>%
          select(
            -rownum
          ) %>%
          relocate(
            Division, .before = Team
          )
        
        #Remove blank rows
        st_df_temp <- st_df_temp[!grepl('* Division', st_df_temp$Team),]
        
      }
      
      st_df  <- rbind.fill(st_df, st_df_temp)
      st_df_temp <- NULL
      
    }
    
    #Set colnames for table
    colnames(gbs_df)[1:30] <- c("Season", "Half", "Conference", "Division", "Team", "GP", "W", "L", "OL", "T",
                                "PTS", "PTS_Pct", "GF", "GA", "SRS", "SOS", "RPt_Pct", "ROW", "RW", "RgRec", "RgPt_Pct")
    
    #Add Playoff col
    st_df <- st_df %>%
      mutate(
        Playoffs = ifelse(grepl('\\*', st_df$Team) == TRUE, "Y", "N")
      ) %>%
      relocate(
        Playoffs, .after = Team
      )
  
  
  #-------------------------#  
  
  
  #---  SCHEDULE & RESULTS  ---#
  
  ## Regular Season
    
    rss_urlstrt <- 'https://www.hockey-reference.com/leagues/NHL_'
    rss_urlend <- '_games.html'
    
    rss_df <- data.frame()
    
    for (i in rss_yrs){
      
      rss_url <- read_html(paste(rss_urlstrt, i, rss_urlend, sep=""))
      
      rss_df_temp <- rss_url %>%
        html_node('table#games') %>%    # select the desired table
        html_table()
      
      #Add cols
      rss_df_temp <- cbind(Season = i, 'Game Type' = 'RS', rss_df_temp)
      
      rss_df <- rbind(rss_df, rss_df_temp)
      rss_df_temp <- NULL
      
    }
    
    colnames(rss_df) <- c('Season', 'Game Type', 'Date', 'Away Team', 'Away Score', 'Home Team', 'Home Score',
                          'Ended', 'Attendance', 'Game Length', 'Notes')
  
  
  #---  CREATE LIST OF ITEMS TO RETURN  ---#
  
  href_dat_output <- list('skater_stats' = skater_season_stats,
                          'goalie_stats' = goalie_season_stats,
                          'league_standings' = st_df,
                          'reg_sched_results' = rss_df
                          )
  
  return(href_dat_output)
  
  ### END ###
  
}

