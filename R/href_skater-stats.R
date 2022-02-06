
# DETAILS -----------------------------------------------------------------

#' Scrape NHL Skater Stats from hockey-reference.com
#'
#' Gathers NHL skater stats by season for the selected seasons and combines results into one dataset.
#' @param seas Which seasons would you like to retrieve data for? No default set. Earliest available season is 1918.
#' @param reg Include regular season data? Defaults to TRUE
#' @param post Include postseason data? Defaults to TRUE
#' @keywords skillalytics
#' @export
#' @examples
#' href_skater_stats(seas = c(1990:1999), reg = TRUE, post = TRUE)


# HREF SKATER STATS -------------------------------------------------------

href_skater_stats <- function(seas, reg = TRUE, post = TRUE){

  # SETUP -------------------------------------------------------------------

    # Initialize Years based on Available Data
    scrape_setup <- skillalytics_setup(seas)
    # Reg
    sbs_yrs <- scrape_setup$sbs_yrs
    sas_yrs <- scrape_setup$sas_yrs
    stoi_yrs <- scrape_setup$stoi_yrs
    sms_yrs <- scrape_setup$sms_yrs
    # Post
    sbps_yrs <- scrape_setup$sbps_yrs
    saps_yrs <- scrape_setup$saps_yrs
    sptoi_yrs <- scrape_setup$sptoi_yrs
    # Team Ref
    tm_ref <- scrape_setup$tm_ref

    # Game Type
    gm_type_reg <- "REG"
    gm_type_post <- "POST"


  # REGULAR SEASON ----------------------------------------------------------

  if(reg == TRUE){

    # REG SBS -----------------------------------------------------------------

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
                           "GP","G", "A", "PTS", "+/-", "PIM", "PS", "EV", "PP", "SH", "GW", "EV.1", "PP.1",
                           "SH.1", "S", "S%", "TOI", "ATOI", "BLK", "HIT", "FOW", "FOL", "FO%"))
      sbs_id_df <- data.frame()

      # Length Variables (used for status message)
      tot_len <- length(sbs_yrs) + length(sas_yrs) + length(stoi_yrs) + length(sms_yrs)
      sbs_len <- length(sbs_yrs)
      sas_len <- length(sas_yrs)
      stoi_len <- length(stoi_yrs)
      sms_len <- length(sms_yrs)
      sbs_num <- 0

      # First Update Message
      tot_strt_time <- tic()
      if(reg == TRUE & post == TRUE){
        message("Note: regular season and playoff stats will be downloaded separatley before being combined and returned as one output")
      }
      message("Downloading NHL regular season skater stats for selected seasons, completion status: ")
      cat(
        paste0(strrep(bgWhite(" "), 49), " ", 0, "%"),
        " \r"
      )

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

        #Set names for temporary df
        #this needs to be done to account for multiple columns with same name (i.e. EV, PP, SH), which in table on site has a second header
        #to indicate Goals vs Assists
        # if statement required to account for different columns based on season
        if (i < 2008) {
          sbs_df_temp <- setNames(sbs_df_temp,
                                   c("Season", "Rk", "Player", "Age", "Tm", "Pos", "GP","G", "A", "PTS", "+/-", "PIM",
                                     "PS", "EV", "PP", "SH", "GW", "EV.1", "PP.1", "SH.1", "S", "S%", "TOI", "ATOI",
                                     "sbs_id_df_temp"))
        } else {
          sbs_df_temp <- setNames(sbs_df_temp,
                                  c("Season", "Rk", "Player", "Age", "Tm", "Pos", "GP","G", "A", "PTS", "+/-", "PIM",
                                    "PS", "EV", "PP", "SH", "GW", "EV.1", "PP.1", "SH.1", "S", "S%", "TOI", "ATOI",
                                    "BLK", "HIT", "FOW", "FOL", "FO%", "sbs_id_df_temp"))
        }
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

        # Print Message to Update User
        sbs_num <- sbs_num + 1
        flush.console()
        cat(
          paste0(
            strrep(bgGreen(green("=")), 50*(sbs_num/tot_len)),
            strrep(bgWhite(" "), 50-(50*(sbs_num/tot_len))),
            " ",
            round((sbs_num/tot_len)*100,digits=0), "%"
          ),
          " \r"
        )

      }

      #Set colnames for table
      colnames(sbs_df)[1:32] <- c('Season', 'Rk', 'Player_Name', 'Age', 'Team_ID', 'Team_Bkdwn_Flag', 'Seas_Sum_Flag', 'Pos1',
                                  'GP', 'G', 'A', 'PTS', 'PlusMinus', 'PIM', 'PS', 'esG', 'ppG', 'shG',	'GWG', 'esA', 'ppA',
                                  'shA', 'S', 'S_Pct', 'TOI', 'Avg_TOI', 'BLK', 'HIT', 'FOW', 'FOL',	'FO_Pct', 'Player_URL')

      # Edit table to convert player url into player id, relocate next to player name, and remove rank col
      sbs_df <- sbs_df %>%
        mutate(
          Player_ID = substr(substring(Player_URL, 12),1,nchar(substring(Player_URL, 12))-5),
          Game_Type = gm_type_reg
        ) %>%
        relocate(
          Player_ID, .before = Player_Name
        ) %>%
        relocate(
          Game_Type, .after = Season
        ) %>%
        select(
          -Rk, -Player_URL
        ) %>%
        arrange(
          Season,
          Player_ID
        )

      # Replace Asterisk from HoF Players with Separate Column
      sbs_df <- sbs_df %>%
        mutate(
          Hof = case_when(
            grepl("\\*", Player_Name) ~ TRUE,
            TRUE ~ FALSE
          ),
          Player_Name = gsub("\\*", "", Player_Name)
        ) %>%
        relocate(
          Hof, .after = Player_Name
        )


    # REG SAS -----------------------------------------------------------------

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
                           c("Season", "Rk", "Player", "Age", "Tm", "Pos", "GP", "CF", "CA", "CF%", "CF% rel", "FF",
                             "FA", "FF%", "FF% rel", "oiSH%", "oiSV%", "PDO", "oZS%", "dZS%", "TOI/60", "TOI(EV)",
                             "TK", "GV", "E+/-", "SAtt.","Thru%"))
        sas_id_df <- data.frame()

        # Length Variables (used for status message)
        sas_num <- 0

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

          # Set names for temporary df
          # if statement required to account for different columns based on season
          if (i < 2015) {
            sas_df_temp <- setNames(sas_df_temp,
                                    c('Season', 'Rk', 'Player', 'Age', 'Tm', 'Pos', 'GP', 'CF', 'CA', 'CF%', 'CF% rel', 'FF',
                                      'FA', 'FF%', 'FF% rel', 'oiSH%', 'oiSV%', 'PDO', 'oZS%', 'dZS%', 'TOI/60', 'TOI(EV)',
                                      'TK', 'GV', 'SAtt.','Thru%', 'sas_id_df_temp'))
          } else {
            sas_df_temp <- setNames(sas_df_temp,
                                    c('Season', 'Rk', 'Player', 'Age', 'Tm', 'Pos', 'GP', 'CF', 'CA', 'CF%', 'CF% rel', 'FF',
                                      'FA', 'FF%', 'FF% rel', 'oiSH%', 'oiSV%', 'PDO', 'oZS%', 'dZS%', 'TOI/60', 'TOI(EV)',
                                      'TK', 'GV', 'E+/-', 'SAtt.','Thru%', 'sas_id_df_temp'))
          }

          #Remove blank rows
          sas_df_temp <- sas_df_temp[!(sas_df_temp$Rk=="Rk"),]

          #Add to full dataframe
          sas_df <- rbind.fill(sas_df, sas_df_temp)

          #Clear temporary df's
          sas_df_temp <- NULL
          sas_id_df <- NULL

          # Print Message to Update User
          sas_num <- sas_num + 1
          flush.console()
          cat(
            paste0(
              strrep(bgGreen(green("=")), 50*((sbs_len+sas_num)/tot_len)),
              strrep(bgWhite(" "), 50-(50*((sbs_len+sas_num)/tot_len))),
              " ",
              round(((sbs_len+sas_num)/tot_len)*100,digits=0), "%"
            ),
            " \r"
          )

        }

        #Set colnames for table
        colnames(sas_df)[1:28] <- c('Season', 'Rk', 'Player_Name', 'Age', 'Team_ID', 'Pos', 'GP', 'esCF', 'esCA', 'esCF_Pct',
                                    'esRelCF_Pct', 'esFF', 'esFA', 'esFF_Pct', 'esRelFF_Pct', 'oiSH_Pct', 'oiSV_Pct', 'PDO',
                                    'oZS_Pct', 'dZS_Pct', 'TOI_per60', 'esTOI_per60', 'TK', 'GV', 'xPlusMinus','TSA',
                                    'SAThruToNet_Pct', 'Player_URL')

        #Breakout Pos col in two based on players who played multiple positions (denoted by "/")
        sas_df <- suppressWarnings(
          separate(data = sas_df, col = Pos, into = c("Pos1", "Pos2"), sep = "/")
        )

        #Edit table to convert player url into player id, relocate next to player name, and remove rank col
        sas_df <- sas_df %>%
          mutate(
            Player_ID = substr(substring(Player_URL, 12),1,nchar(substring(Player_URL, 12))-5),
            Game_Type = gm_type_reg
          ) %>%
          relocate(
            Player_ID, .before = Player_Name
          ) %>%
          relocate(
            Game_Type, .after = Season
          ) %>%
          select(
            -Rk, -Player_URL
          ) %>%
          arrange(
            Season,
            Player_ID
          )

        # Replace Asterisk from HoF Players with Separate Column
        sas_df <- sas_df %>%
          mutate(
            Hof = case_when(
              grepl("\\*", Player_Name) ~ TRUE,
              TRUE ~ FALSE
            ),
            Player_Name = gsub("\\*", "", Player_Name)
          ) %>%
          relocate(
            Hof, .after = Player_Name
          )

      } else {
        #skip
      }


    # REG STOI ----------------------------------------------------------------

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
        stoi_df <- setNames(data.frame(matrix(ncol = 20, nrow = 0)),
                            c('Season', 'Rk', 'Player_Name', 'Team', 'Pos', 'Avg_ShiftLength_perGm', 'GP', 'esTOI_perGm',
                              'esRelCF_Pct', 'esGF_per60', 'esGA_per60', 'ppTOI_perGm', 'ppRelCF_Pct', 'ppGF_per60',
                              'ppGA_per60', 'shTOI_perGm', 'shRelCF_Pct', 'shGF_per60', 'shGA_per60', 'Player_URL'))
        stoi_id_df <- data.frame()

        # Length Variables (used for status message)
        stoi_num <- 0

        #Scrape data
        for (i in stoi_yrs){

          #Create url to scrape
          stoi_url <- read_html(paste(stoi_urlstrt, i, stoi_urlend, sep=""))

          #Get table
          stoi_df_temp <- stoi_url %>%
            html_node('table#stats_toi') %>%    # select the desired table
            html_table()

          # Remove NA/Blank Columns
          blank_cols <- which(is.na(stoi_df_temp[1,]))
          stoi_df_temp <- stoi_df_temp[-c(blank_cols)]

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

          # Set names for temporary df
          stoi_df_temp <- setNames(stoi_df_temp,
                                   c('Season', 'Rk', 'Player_Name', 'Team', 'Pos', 'Avg_ShiftLength_perGm', 'GP', 'esTOI_perGm',
                                     'esRelCF_Pct', 'esGF_per60', 'esGA_per60', 'ppTOI_perGm', 'ppRelCF_Pct', 'ppGF_per60',
                                     'ppGA_per60', 'shTOI_perGm', 'shRelCF_Pct', 'shGF_per60', 'shGA_per60', 'Player_URL')
                                   )

          # Change URL Column to String
          stoi_df_temp <- stoi_df_temp %>% mutate(Player_URL = as.character(Player_URL))

          #Remove blank rows
          stoi_df_temp <- stoi_df_temp[!(stoi_df_temp$Rk=="Rk"),]

          #Add to full dataframe
          stoi_df <- rbind.fill(stoi_df, stoi_df_temp)

          #Clear temporary df's
          stoi_df_temp <- NULL
          stoi_id_df <- NULL

          # Print Message to Update User
          stoi_num <- stoi_num + 1
          flush.console()
          cat(
            paste0(
              strrep(bgGreen(green("=")), 50*((sbs_len+sas_len+stoi_num)/tot_len)),
              strrep(bgWhite(" "), 50-(50*((sbs_len+sas_len+stoi_num)/tot_len))),
              " ",
              round(((sbs_len+sas_len+stoi_num)/tot_len)*100,digits=0), "%"
            ),
            " \r"
          )

        }

        #Set colnames for table
        colnames(stoi_df)[1:20] <- c('Season', 'Rk', 'Player_Name', 'Team_ID', 'Pos', 'Avg_ShiftLength_perGm', 'GP', 'esTOI_perGm',
                                     'esRelCF_Pct', 'esGF_per60', 'esGA_per60', 'ppTOI_perGm', 'ppRelCF_Pct', 'ppGF_per60',
                                     'ppGA_per60', 'shTOI_perGm', 'shRelCF_Pct', 'shGF_per60', 'shGA_per60', 'Player_URL')

        #Breakout Pos col in two based on players who played multiple positions (denoted by "/")
        stoi_df <- suppressWarnings(
          separate(data = stoi_df, col = Pos, into = c("Pos1", "Pos2"), sep = "/")
          )

        #Edit table to convert player url into player id, relocate next to player name, and remove rank col
        stoi_df <- stoi_df %>%
          mutate(
            Player_ID = substr(substring(Player_URL, 12),1,nchar(substring(Player_URL, 12))-5),
            Game_Type = gm_type_reg
          ) %>%
          relocate(
            Player_ID, .before = Player_Name
          ) %>%
          relocate(
            Game_Type, .after = Season
          ) %>%
          select(
            -Rk, -Player_URL
          ) %>%
          arrange(
            Season,
            Player_ID
          )

        # Replace Asterisk from HoF Players with Separate Column
        stoi_df <- stoi_df %>%
          mutate(
            Hof = case_when(
              grepl("\\*", Player_Name) ~ TRUE,
              TRUE ~ FALSE
            ),
            Player_Name = gsub("\\*", "", Player_Name)
          ) %>%
          relocate(
            Hof, .after = Player_Name
          )

      } else {
        #skip
      }


    # REG SMS -----------------------------------------------------------------

      #Create table url variables
      sms_urlstrt <- 'https://www.hockey-reference.com/leagues/NHL_'
      sms_urlend <- '_skaters-misc.html'

      #Create xpath variables to get player id
      sms_xpath_strt <- '//*[@id="stats_misc"]/tbody/tr['
      sms_xpath_mid <- ']/td['
      sms_xpath_end <- ']/a'

      #Create empty dataframe to fill
      # sms_df <- setNames(data.frame(matrix(ncol = 33, nrow = 0)),
      #                    c('Season', as.character('Rk'), 'Player', 'Age', 'Tm', 'Pos', 'GP', 'GC', 'G', 'A', 'PTS', 'GC', 'PIM',
      #                      'S', 'G', 'A', 'PTS', 'GC', 'TGF', 'PGF', 'TGA', 'PGA', '+/-', 'xGF', 'xGA', 'E+/-',
      #                      'OPS', 'DPS', 'PS', 'Att.', 'Made', 'Miss', 'Pct.')
      #                    )
      sms_df <- data.frame("Season" = integer(),
                           "Rk" = character(),
                           "Player" = character(),
                           "Age" = character(),
                           "Tm" = character(),
                           "Pos" = character(),
                           "GP" = character(),
                           "GC" = character(),
                           "G" = character(),
                           "A" = character(),
                           "PTS" = character(),
                           "GC" = character(),
                           "PIM" = character(),
                           "S" = character(),
                           "G" = character(),
                           "A" = character(),
                           "PTS" = character(),
                           "GC" = character(),
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

      # Length Variables (used for status message)
      sms_num <- 0

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

        # Set names for temporary df
        # if statement required to account for different columns based on season
        if (i < 2006) {
          sms_df_temp <- setNames(sms_df_temp,
                                  c('Season', 'Rk', 'Player', 'Age', 'Tm', 'Pos', 'GP', 'GC', 'G', 'A', 'PTS', 'GC', 'PIM',
                                    'S', 'G', 'A', 'PTS', 'GC', 'TGF', 'PGF', 'TGA', 'PGA', '+/-', 'OPS', 'DPS', 'PS',
                                    'sas_id_df_temp')
          )
        } else  if (i < 2015) {
          sms_df_temp <- setNames(sms_df_temp,
                                  c('Season', 'Rk', 'Player', 'Age', 'Tm', 'Pos', 'GP', 'GC', 'G', 'A', 'PTS', 'GC', 'PIM',
                                    'S', 'G', 'A', 'PTS', 'GC', 'TGF', 'PGF', 'TGA', 'PGA', '+/-', 'OPS', 'DPS', 'PS', 'Att.',
                                    'Made', 'Miss', 'Pct.', 'sas_id_df_temp')
                                  )
        } else {
          sms_df_temp <- setNames(sms_df_temp,
                                  c('Season', 'Rk', 'Player', 'Age', 'Tm', 'Pos', 'GP', 'GC', 'G', 'A', 'PTS', 'GC', 'PIM',
                                    'S', 'G', 'A', 'PTS', 'GC', 'TGF', 'PGF', 'TGA', 'PGA', '+/-', 'xGF', 'xGA', 'E+/-',
                                    'OPS', 'DPS', 'PS', 'Att.', 'Made', 'Miss', 'Pct.', 'sas_id_df_temp')
                                  )
        }

        #Remove blank rows
        sms_df_temp <- sms_df_temp[!(sms_df_temp$Rk=="Rk"),]

        #Add to full dataframe
        sms_df <- suppressMessages(
          bind_rows(sms_df, sms_df_temp)
        )

        #Clear temporary df's
        sms_df_temp <- NULL
        sms_id_df <- NULL

        # Print Message to Update User
        sms_num <- sms_num + 1
        flush.console()
        cat(
          paste0(
            strrep(bgGreen(green("=")), 50*((sbs_len+sas_len+stoi_len+sms_num)/tot_len)),
            strrep(bgWhite(" "), 50-(50*((sbs_len+sas_len+stoi_len+sms_num)/tot_len))),
            " ",
            round(((sbs_len+sas_len+stoi_len+sms_num)/tot_len)*100,digits=0), "%"
          ),
          " \r"
        )

      }

      #Set colnames for table
      colnames(sms_df)[1:34] <- c('Season', 'Rk', 'Player_Name', 'Age', 'Team_ID', 'Pos', 'GP', 'GC', 'G_perGm', 'A_perGm', 'PTS_perGm', 'GC_perGm',
                                  'PIM_perGm', 'S_perGm', 'adjG', 'adjA', 'adjPTS', 'adjGC', 'oiTGF', 'oiPPGF', 'oiTGA', 'oiPKGA', 'PlusMinus',
                                  'xGF', 'xGA', 'xPlusMinus', 'OPS', 'DPS', 'PS', 'soAtt', 'soMade', 'soMiss', 'soPct', 'Player_URL')

      #Breakout Pos col in two based on players who played multiple positions (denoted by "/")
      sms_df <- suppressWarnings(
        separate(data = sms_df, col = Pos, into = c("Pos1", "Pos2"), sep = "/")
      )

      #Edit table to convert player url into player id, relocate next to player name, and remove rank col
      sms_df <- sms_df %>%
        mutate(
          Player_ID = substr(substring(Player_URL, 12),1,nchar(substring(Player_URL, 12))-5),
          Game_Type = gm_type_reg
        ) %>%
        relocate(
          Player_ID, .before = Player_Name
        ) %>%
        relocate(
          Game_Type, .after = Season
        ) %>%
        select(
          -Rk, -Player_URL
        ) %>%
        arrange(
          Season,
          Player_ID
        )

      # Replace Asterisk from HoF Players with Separate Column
      sms_df <- sms_df %>%
        mutate(
          Hof = case_when(
            grepl("\\*", Player_Name) ~ TRUE,
            TRUE ~ FALSE
          ),
          Player_Name = gsub("\\*", "", Player_Name)
        ) %>%
        relocate(
          Hof, .after = Player_Name
        )


    # REG TBL COMBINE ---------------------------------------------------------

      skater_reg_stats <- sbs_df

      if (length(sas_yrs) != 0) {

        #Add advanced table to basic stats
        skater_reg_stats <- skater_reg_stats %>%
          left_join(
            sas_df[, c('Season', 'Player_ID', 'Team_ID', 'Pos2', 'esCF', 'esCA', 'esCF_Pct', 'esRelCF_Pct', 'esFF', 'esFA',
                       'esFF_Pct', 'esRelFF_Pct', 'oiSH_Pct', 'oiSV_Pct', 'PDO', 'oZS_Pct', 'dZS_Pct', 'TOI_per60',
                       'esTOI_per60', 'TK', 'GV', 'xPlusMinus','TSA','SAThruToNet_Pct')],
            by = c('Season', 'Player_ID', 'Team_ID')
          ) %>%
          relocate(
            Pos2, .after = Pos1
          )

      } else {
        #skip
      }

      if (length(stoi_yrs) != 0) {

        #Add TOI table
        skater_reg_stats <- skater_reg_stats %>%
          left_join(
            stoi_df[, c('Season', 'Player_ID', 'Team_ID', 'Avg_ShiftLength_perGm', 'esTOI_perGm', 'esGF_per60', 'esGA_per60', 'ppTOI_perGm',
                        'ppRelCF_Pct', 'ppGF_per60', 'ppGA_per60', 'shTOI_perGm', 'shRelCF_Pct', 'shGF_per60', 'shGA_per60')],
            by = c('Season', 'Player_ID', 'Team_ID')
          )

      } else {
        #skip
      }

      #Add Misc table
      skater_reg_stats <- skater_reg_stats %>%
        left_join(
          sms_df[, c('Season', 'Player_ID', 'Team_ID', 'GC', 'G_perGm', 'A_perGm', 'PTS_perGm', 'GC_perGm', 'PIM_perGm',
                     'S_perGm', 'adjG', 'adjA', 'adjPTS', 'adjGC', 'oiTGF', 'oiPPGF', 'oiTGA', 'oiPKGA', 'xGF', 'xGA',
                     'OPS', 'DPS', 'soAtt', 'soMade', 'soMiss', 'soPct')],
          by = c('Season', 'Player_ID', 'Team_ID')
        )

    # Add Team ID from Reference List
    skater_reg_stats <- skater_reg_stats %>%
      left_join(
        tm_ref,
        by = 'Team_ID'
      ) %>%
      relocate(
        Team_Name,
        .after = Team_ID
      )

    # Print Final Update Message
    tot_end_time <- tic()
    tot_time_diff <- tot_end_time - tot_strt_time
    cat(
      paste0(strrep(bgGreen(green("=")), 49), " ", 100, "%"),
      " \n"
    )
    message(
      paste0("Download successfully completed!")
    )
    cat(
      "Time taken:",
      sprintf("%2i", hour(seconds_to_period(tot_time_diff))),
      "hr",
      sprintf("%2i", minute(seconds_to_period(tot_time_diff))),
      "min",
      round(tot_time_diff%%60,0),
      "sec",
      " \n"
    )

  # If reg = FALSE
  } else {

    skater_reg_stats <- data.frame()

  }


  # PLAYOFFS ----------------------------------------------------------------

  if(post == TRUE){

    # POST SBS ----------------------------------------------------------------

      # Create Table URL Variables
      sbps_urlstrt <- 'https://www.hockey-reference.com/playoffs/NHL_'
      sbps_urlend <- '_skaters.html'

      # Create XPath Variables to Get Player ID
      sbps_xpath_strt <- '//*[@id="stats"]/tbody/tr['
      sbps_xpath_mid <- ']/td['
      sbps_xpath_end <- ']/a'

      # Create Empty Dataframe to Fill
      sbps_df <- setNames(data.frame(matrix(ncol = 28, nrow = 0)),
                          c("Season", "Rk", "Player", "Age", "Tm", "Pos",
                            "GP","G", "A", "PTS", "+/-", "PIM", "EV", "PP", "SH", "GW", "EV.1", "PP.1", "SH.1",
                            "S", "S%", "TOI", "ATOI", "BLK", "HIT", "FOW", "FOL", "FO%"))
      sbps_id_df <- data.frame()

      # Length Variables (used for status message)
      tot_len <- length(sbps_yrs) + length(saps_yrs) + length(sptoi_yrs)
      sbps_len <- length(sbps_yrs)
      saps_len <- length(saps_yrs)
      sptoi_len <- length(sptoi_yrs)
      sbps_num <- 0

      # First Update Message
      tot_strt_time <- tic()
      message("Downloading NHL skater playoff stats for selected seasons, completion status: ")
      cat(
        paste0(strrep(bgWhite(" "), 49), " ", 0, "%"),
        " \r"
      )

      # Scrape Data
      for (i in sbps_yrs){

        # Create URL to Scrape
        sbps_url <- read_html(paste(sbps_urlstrt, i, sbps_urlend, sep=""))

        # Get Table
        sbps_df_temp <- sbps_url %>%
          html_node('table#stats') %>%    # select the desired table
          html_table()

        # Edit Column Names
        names(sbps_df_temp) <- as.matrix(sbps_df_temp[1, ])
        sbps_df_temp <- sbps_df_temp[-1, ]
        sbps_df_temp <- cbind(Season = i, sbps_df_temp)

        # Get Number of Players in Table (for use in next loop)
        rows <- (1:nrow(sbps_df_temp))

        # Loop through each row of table to get player url (to be used for Player ID)
        for (l in rows) {

          # Get URL (Player ID)
          sbps_id_df_temp <- sbps_url %>%
            html_node(
              xpath = paste(sbps_xpath_strt, l, sbps_xpath_mid, 1, sbps_xpath_end, sep = "")
            ) %>%
            html_attr('href')

          # Turn into Dataframe to Combine with Stats Table
          sbps_id_df_temp <- data.frame(sbps_id_df_temp)
          sbps_id_df <- rbind(sbps_id_df, sbps_id_df_temp)

        }

        # Combine Data
        sbps_df_temp <- cbind(sbps_df_temp, sbps_id_df)

        # Find rows of players that have multiple listings based on playing for more than one team in season
        sbps_tots <- sbps_df_temp %>%
          select(
            Season, sbps_id_df_temp, Tm
          ) %>%
          subset(
            Tm == 'TOT'
          )

        # Set Names for Temporary Dataframe
        # this needs to be done to account for multiple columns with same name (i.e. EV, PP, SH), which in table on site has a second header
        # to indicate Goals vs Assists
        # if statement required to account for different columns based on season
        if (i < 2008) {
          sbps_df_temp <- setNames(sbps_df_temp,
                                   c("Season", "Rk", "Player", "Age", "Tm", "Pos", "GP","G", "A", "PTS", "+/-", "PIM",
                                     "EV", "PP", "SH", "GW", "EV.1", "PP.1", "SH.1", "S", "S%", "TOI", "ATOI",
                                     "sbps_id_df_temp"))
        } else {
          sbps_df_temp <- setNames(sbps_df_temp,
                                   c("Season", "Rk", "Player", "Age", "Tm", "Pos", "GP","G", "A", "PTS", "+/-", "PIM",
                                     "EV", "PP", "SH", "GW", "EV.1", "PP.1", "SH.1", "S", "S%", "TOI", "ATOI",
                                     "BLK", "HIT", "FOW", "FOL", "FO%", "sbps_id_df_temp"))
        }

        # Remove Blank Rows
        sbps_df_temp <- sbps_df_temp[!(sbps_df_temp$Rk=="Rk"),]

        # Add to Full Dataframe
        sbps_df <- rbind.fill(sbps_df, sbps_df_temp)

        # Clear Temporary Dataframes
        sbps_df_temp <- NULL
        sbps_id_df <- NULL

        # Print Message to Update User
        sbps_num <- sbps_num + 1
        flush.console()
        cat(
          paste0(
            strrep(bgGreen(green("=")), 50*(sbps_num/tot_len)),
            strrep(bgWhite(" "), 50-(50*(sbps_num/tot_len))),
            " ",
            round((sbps_num/tot_len)*100,digits=0), "%"
          ),
          " \r"
        )

      }

      # Set Column Name for Table
      colnames(sbps_df)[1:29] <- c('Season', 'Rk', 'Player_Name', 'Age', 'Team_ID', 'Pos1',
                                   'GP', 'G', 'A', 'PTS', 'PlusMinus', 'PIM', 'esG', 'ppG', 'shG',	'GWG', 'esA', 'ppA', 'shA',
                                   'S', 'S_Pct', 'TOI', 'Avg_TOI', 'BLK', 'HIT', 'FOW', 'FOL',	'FO_Pct', 'Player_URL')

      # Edit table to convert player url into player id, relocate next to player name, and remove rank col
      sbps_df <- sbps_df %>%
        mutate(
          Player_ID = substr(substring(Player_URL, 12),1,nchar(substring(Player_URL, 12))-5),
          Game_Type = gm_type_post
        ) %>%
        relocate(
          Player_ID, .before = Player_Name
        ) %>%
        relocate(
          Game_Type, .after = Season
        ) %>%
        select(
          -Rk, -Player_URL
        ) %>%
        arrange(
          Season,
          Player_ID
        )

      # Replace Asterisk from HoF Players with Separate Column
      sbps_df <- sbps_df %>%
        mutate(
          Hof = case_when(
            grepl("\\*", Player_Name) ~ TRUE,
            TRUE ~ FALSE
          ),
          Player_Name = gsub("\\*", "", Player_Name)
        ) %>%
        relocate(
          Hof, .after = Player_Name
        )


    # POST SAS ----------------------------------------------------------------

      # Check if table should be skipped based on years selected
      if (length(saps_yrs) != 0) {

        # Create Table URL Variables
        saps_urlstrt <- 'https://www.hockey-reference.com/playoffs/NHL_'
        saps_urlend <- '_skaters-advanced.html'

        # Create Xpath Variables to Get Player ID
        saps_xpath_strt <- '//*[@id="stats_adv_pl"]/tbody/tr['
        saps_xpath_mid <- ']/td['
        saps_xpath_end <- ']/a'

        # Create Empty Dataframe to Fill
        saps_df <- setNames(data.frame(matrix(ncol = 27, nrow = 0)),
                            c("Season", "Rk", "Player", "Age", "Tm", "Pos", "GP", "CF", "CA", "CF%", "CF% rel", "FF", "FA", "FF%",
                              "FF% rel", "oiSH%", "oiSV%", "PDO", "oZS%", "dZS%", "TOI/60", "TOI(EV)", "TK", "GV", "E+/-", "SAtt.", "Thru%"))
        saps_id_df <- data.frame()

        # Length Variables (used for status message)
        saps_num <- 0

        # Scrape Data
        for (i in saps_yrs){

          # Create URL to Scrape
          saps_url <- read_html(paste(saps_urlstrt, i, saps_urlend, sep=""))

          # Get Table
          saps_df_temp <- saps_url %>%
            html_node('table#stats_adv_pl') %>%    # select the desired table
            html_table()

          # Edit Column Names
          names(saps_df_temp) <- as.matrix(saps_df_temp[1, ])
          saps_df_temp <- saps_df_temp[-1, ]
          saps_df_temp <- cbind(Season = i, saps_df_temp)

          # Get Number of Players in Table (for use in next loop)
          rows <- (1:nrow(saps_df_temp))

          # Loop through each row of table to get player url (to be used for player id)
          for (l in rows) {

            # Get URL (Player ID)
            saps_id_df_temp <- saps_url %>%
              html_node(
                xpath = paste(saps_xpath_strt, l, saps_xpath_mid, 1, saps_xpath_end, sep = "")
              ) %>%
              html_attr('href')

            # Turn into Dataframe to Combine with Stats Table
            saps_id_df_temp <- data.frame(saps_id_df_temp)
            saps_id_df <- rbind(saps_id_df, saps_id_df_temp)

          }

          # Combine Data
          saps_df_temp <- cbind(saps_df_temp, saps_id_df)

          # Set names for temporary df
          # if statement required to account for different columns based on season
          if (i < 2015) {
            saps_df_temp <- setNames(saps_df_temp,
                                    c('Season', 'Rk', 'Player', 'Age', 'Tm', 'Pos', 'GP', 'CF', 'CA', 'CF%', 'CF% rel', 'FF',
                                      'FA', 'FF%', 'FF% rel', 'oiSH%', 'oiSV%', 'PDO', 'oZS%', 'dZS%', 'TOI/60', 'TOI(EV)',
                                      'TK', 'GV', 'SAtt.','Thru%', 'sas_id_df_temp'))
          } else {
            saps_df_temp <- setNames(saps_df_temp,
                                    c('Season', 'Rk', 'Player', 'Age', 'Tm', 'Pos', 'GP', 'CF', 'CA', 'CF%', 'CF% rel', 'FF',
                                      'FA', 'FF%', 'FF% rel', 'oiSH%', 'oiSV%', 'PDO', 'oZS%', 'dZS%', 'TOI/60', 'TOI(EV)',
                                      'TK', 'GV', 'E+/-', 'SAtt.','Thru%', 'sas_id_df_temp'))
          }

          # Remove Blank Rows
          saps_df_temp <- saps_df_temp[!(saps_df_temp$Rk=="Rk"),]

          # Add to Full Dataframe
          saps_df <- rbind.fill(saps_df, saps_df_temp)

          # Clear Temporary Dataframes
          saps_df_temp <- NULL
          saps_id_df <- NULL

          # Print Message to Update User
          saps_num <- saps_num + 1
          flush.console()
          cat(
            paste0(
              strrep(bgGreen(green("=")), 50*((sbps_len+saps_num)/tot_len)),
              strrep(bgWhite(" "), 50-(50*((sbps_len+saps_num)/tot_len))),
              " ",
              round(((sbps_len+saps_num)/tot_len)*100,digits=0), "%"
            ),
            " \r"
          )

        }

        # Set Column Names for Table
        colnames(saps_df)[1:28] <- c('Season', 'Rk', 'Player_Name', 'Age', 'Team_ID', 'Pos', 'GP',
                                     'esCF', 'esCA', 'esCF_Pct', 'esRelCF_Pct', 'esFF', 'esFA', 'esFF_Pct', 'esRelFF_Pct',
                                     'oiSH_Pct', 'oiSV_Pct', 'PDO', 'oZS_Pct', 'dZS_Pct', 'TOI_per60', 'esTOI_per60', 'TK', 'GV',
                                     'xPlusMinus','TSA','SAThruToNet_Pct', 'Player_URL')

        # Breakout Pos col in two based on players who played multiple positions (denoted by "/")
        saps_df <- suppressWarnings(
          separate(data = saps_df, col = Pos, into = c("Pos1", "Pos2"), sep = "/")
        )

        # Edit table to convert player url into player id, relocate next to player name, and remove rank col
        saps_df <- saps_df %>%
          mutate(
            Player_ID = substr(substring(Player_URL, 12),1,nchar(substring(Player_URL, 12))-5),
            Game_Type = gm_type_post
          ) %>%
          relocate(
            Player_ID, .before = Player_Name
          ) %>%
          relocate(
            Game_Type, .after = Season
          ) %>%
          select(
            -Rk, -Player_URL
          ) %>%
          arrange(
            Season,
            Player_ID
          )

        # Replace Asterisk from HoF Players with Separate Column
        saps_df <- saps_df %>%
          mutate(
            Hof = case_when(
              grepl("\\*", Player_Name) ~ TRUE,
              TRUE ~ FALSE
            ),
            Player_Name = gsub("\\*", "", Player_Name)
          ) %>%
          relocate(
            Hof, .after = Player_Name
          )

      } else {
        #skip
      }


    # POST TOI ----------------------------------------------------------------

      # Check if table should be skipped based on years selected
      if (length(sptoi_yrs) != 0) {

        # Create Create Table URL Variables
        sptoi_urlstrt <- 'https://www.hockey-reference.com/playoffs/NHL_'
        sptoi_urlend <- '_skaters-time-on-ice.html'

        # Create Xpath Variables to Get Player ID
        sptoi_xpath_strt <- '//*[@id="stats_toi"]/tbody/tr['
        sptoi_xpath_mid <- ']/td['
        sptoi_xpath_end <- ']/a'

        # Create Empty Dataframe to Fill
        sptoi_df <- setNames(data.frame(matrix(ncol = 20, nrow = 0)),
                             c('Season', 'Rk', 'Player_Name', 'Team', 'Pos', 'Avg_ShiftLength_perGm', 'GP', 'esTOI_perGm',
                               'esRelCF_Pct', 'esGF_per60', 'esGA_per60', 'ppTOI_perGm', 'ppRelCF_Pct', 'ppGF_per60',
                               'ppGA_per60', 'shTOI_perGm', 'shRelCF_Pct', 'shGF_per60', 'shGA_per60', 'Player_URL')
                             )
        sptoi_id_df <- data.frame()

        # Length Variables (used for status message)
        sptoi_num <- 0

        # Scrape Data
        for (i in sptoi_yrs){

          # Create URL to Scrape
          sptoi_url <- read_html(paste(sptoi_urlstrt, i, sptoi_urlend, sep=""))

          # Get Table
          sptoi_df_temp <- sptoi_url %>%
            html_node('table#stats_toi') %>%    # select the desired table
            html_table()

          # Remove NA/Blank Columns
          blank_post_cols <- which(is.na(sptoi_df_temp[1,]))
          sptoi_df_temp <- sptoi_df_temp[-c(blank_post_cols)]

          # Edit Column Names
          names(sptoi_df_temp) <- as.matrix(sptoi_df_temp[1, ])
          sptoi_df_temp <- sptoi_df_temp[-1, ]
          sptoi_df_temp <- cbind(Season = i, sptoi_df_temp)

          # Get Number of Players in Table (for use in next loop)
          rows <- (1:nrow(sptoi_df_temp))

          # Loop through each row of table to get player url (to be used for player id)
          for (l in rows) {

            # Get URL (Player ID)
            sptoi_id_df_temp <- sptoi_url %>%
              html_node(
                xpath = paste(sptoi_xpath_strt, l, sptoi_xpath_mid, 1, sptoi_xpath_end, sep = "")
              ) %>%
              html_attr('href')

            # Turn into Dataframe to Combine with Stats Table
            sptoi_id_df_temp <- data.frame(sptoi_id_df_temp)
            sptoi_id_df <- rbind(sptoi_id_df, sptoi_id_df_temp)

          }

          # Combine Data
          sptoi_df_temp <- cbind(sptoi_df_temp, sptoi_id_df)

          # Set names for temporary df
          sptoi_df_temp <- setNames(sptoi_df_temp,
                                   c('Season', 'Rk', 'Player_Name', 'Team', 'Pos', 'Avg_ShiftLength_perGm', 'GP', 'esTOI_perGm',
                                     'esRelCF_Pct', 'esGF_per60', 'esGA_per60', 'ppTOI_perGm', 'ppRelCF_Pct', 'ppGF_per60',
                                     'ppGA_per60', 'shTOI_perGm', 'shRelCF_Pct', 'shGF_per60', 'shGA_per60', 'Player_URL')
                                   )

          # Change URL Column to String
          sptoi_df_temp <- sptoi_df_temp %>% mutate(Player_URL = as.character(Player_URL))

          # Remove Blank Rows
          sptoi_df_temp <- sptoi_df_temp[!(sptoi_df_temp$Rk=="Rk"),]

          # Add to Full Dataframe
          sptoi_df <- rbind.fill(sptoi_df, sptoi_df_temp)

          # Clear Temporary Dataframes
          sptoi_df_temp <- NULL
          sptoi_id_df <- NULL

          # Print Message to Update User
          sptoi_num <- sptoi_num + 1
          flush.console()
          cat(
            paste0(
              strrep(bgGreen(green("=")), 50*((sbps_len+saps_len+sptoi_num)/tot_len)),
              strrep(bgWhite(" "), 50-(50*((sbps_len+saps_len+sptoi_num)/tot_len))),
              " ",
              round(((sbps_len+saps_len+sptoi_num)/tot_len)*100,digits=0), "%"
            ),
            " \r"
          )

        }

        # Set Column Names for Table
        colnames(sptoi_df)[1:20] <- c('Season', 'Rk', 'Player_Name', 'Team_ID', 'Pos', 'Avg_ShiftLength_perGm', 'GP',
                                      'esTOI_perGm', 'esRelCF_Pct', 'esGF_per60', 'esGA_per60', 'ppTOI_perGm', 'ppRelCF_Pct',
                                      'ppGF_per60', 'ppGA_per60', 'shTOI_perGm', 'shRelCF_Pct', 'shGF_per60', 'shGA_per60', 'Player_URL')

        # Breakout Pos col in two based on players who played multiple positions (denoted by "/")
        sptoi_df <- suppressWarnings(
          separate(data = sptoi_df, col = Pos, into = c("Pos1", "Pos2"), sep = "/")
        )

        # Edit table to convert player url into player id, relocate next to player name, and remove rank col
        sptoi_df <- sptoi_df %>%
          mutate(
            Player_ID = substr(substring(Player_URL, 12),1,nchar(substring(Player_URL, 12))-5),
            Game_Type = gm_type_post
          ) %>%
          relocate(
            Player_ID, .before = Player_Name
          ) %>%
          relocate(
            Game_Type, .after = Season
          ) %>%
          select(
            -Rk, -Player_URL
          ) %>%
          arrange(
            Season,
            Player_ID
          )

        # Replace Asterisk from HoF Players with Separate Column
        sptoi_df <- sptoi_df %>%
          mutate(
            Hof = case_when(
              grepl("\\*", Player_Name) ~ TRUE,
              TRUE ~ FALSE
            ),
            Player_Name = gsub("\\*", "", Player_Name)
          ) %>%
          relocate(
            Hof, .after = Player_Name
          )

      } else {
        #skip
      }


    # POST TBL COMBINE --------------------------------------------------------

      skater_post_stats <- sbps_df

      if (length(saps_yrs) != 0) {

        #Add advanced table to basic stats
        skater_post_stats <- skater_post_stats %>%
          left_join(
            saps_df[, c('Season', 'Player_ID', 'Team_ID', 'Pos2', 'esCF', 'esCA', 'esCF_Pct', 'esRelCF_Pct', 'esFF', 'esFA',
                        'esFF_Pct', 'esRelFF_Pct', 'oiSH_Pct', 'oiSV_Pct', 'PDO', 'oZS_Pct', 'dZS_Pct', 'TOI_per60',
                        'esTOI_per60', 'TK', 'GV', 'xPlusMinus','TSA','SAThruToNet_Pct')],
            by = c('Season', 'Player_ID', 'Team_ID')
          ) %>%
          relocate(
            Pos2, .after = Pos1
          )

      } else {
        #skip
      }

      if (length(sptoi_yrs) != 0) {

        #Add TOI table
        skater_post_stats <- skater_post_stats %>%
          left_join(
            sptoi_df[, c('Season', 'Player_ID', 'Team_ID', 'Avg_ShiftLength_perGm', 'esTOI_perGm', 'esGF_per60', 'esGA_per60', 'ppTOI_perGm',
                         'ppRelCF_Pct', 'ppGF_per60', 'ppGA_per60', 'shTOI_perGm', 'shRelCF_Pct', 'shGF_per60', 'shGA_per60')],
            by = c('Season', 'Player_ID', 'Team_ID')
          )

      } else {
        #skip
      }

    # Add Team ID from Reference List
    skater_post_stats <- skater_post_stats %>%
      left_join(
        tm_ref,
        by = 'Team_ID'
      ) %>%
      relocate(
        Team_Name,
        .after = Team_ID
      )

    # Print Final Update Message
    tot_end_time <- tic()
    tot_time_diff <- tot_end_time - tot_strt_time
    cat(
      paste0(strrep(bgGreen(green("=")), 49), " ", 100, "%"),
      " \n"
    )
    message(
      paste0("Download successfully completed!")
    )
    cat(
      "Time taken:",
      sprintf("%2i", hour(seconds_to_period(tot_time_diff))),
      "hr",
      sprintf("%2i", minute(seconds_to_period(tot_time_diff))),
      "min",
      round(tot_time_diff%%60,0),
      "sec",
      " \n"
    )

  # If post = FALSE
  } else {

    skater_post_stats <- data.frame()

  }


  # RETURN OUTPUT -----------------------------------------------------------

    # Combine with Postseason Dataframe
    skater_stats <- rbind.fill(skater_reg_stats, skater_post_stats)

    # Return Full Output
    return(skater_stats)

} #end function


