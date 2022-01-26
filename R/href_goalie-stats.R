
# DETAILS -----------------------------------------------------------------

#' Scrape NHL Goalie Stats from hockey-reference.com
#'
#' Gathers NHL goalie stats by season for the selected seasons and combines results into one dataset.
#' @param seas Which seasons would you like to retrieve data for? No default set.
#' @param reg Include regular season data? Defaults to TRUE
#' @param post Include postseason data? Defaults to TRUE
#' @keywords skillalytics
#' @export
#' @examples
#' href_goalie_stats()


# HREF GOALIE STATS -------------------------------------------------------

href_goalie_stats <- function(seas, reg = TRUE, post = TRUE){

  # SETUP -------------------------------------------------------------------

    # Initialize Years based on Available Data
    scrape_setup <- skillalytics_setup(seas)
    gbs_yrs <- scrape_setup$gbs_yrs
    gso_yrs <- scrape_setup$gso_yrs
    gbps_yrs <- scrape_setup$gbps_yrs

    # Game Type
    gm_type_reg <- "REG"
    gm_type_post <- "POST"


  # REGULAR SEASON ----------------------------------------------------------

  if(reg == TRUE){

    # REG GBS -----------------------------------------------------------------

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

      # Length Variables (used for status message)
      tot_len <- length(gbs_yrs) + length(gso_yrs)
      gbs_len <- length(gbs_yrs)
      gso_len <- length(gso_yrs)
      gbs_num <- 0

      # First Update Message
      tot_strt_time <- tic()
      if(reg == TRUE & post == TRUE){
        message("Note: regular season and playoff stats will be downloaded separatley before being combined and returned as one output")
      }
      message("Downloading NHL regular season goalie stats for selected seasons, completion status: ")
      cat(
        paste0(strrep(bgWhite(" "), 49), " ", 0, "%"),
        " \r"
      )

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

        # Print Message to Update User
        gbs_num <- gbs_num + 1
        flush.console()
        cat(
          paste0(
            strrep(bgGreen(green("=")), 50*(gbs_num/tot_len)),
            strrep(bgWhite(" "), 50-(50*(gbs_num/tot_len))),
            " ",
            round((gbs_num/tot_len)*100,digits=0), "%"
          ),
          " \r"
        )

      }

      #Set colnames for table
      colnames(gbs_df)[1:30] <- c('Season', 'Rk', 'Player_Name', 'Age', 'Team',  'Team_Bkdwn_Flag', 'Seas_Sum_Flag', 'GP', 'GS', 'W', 'L',
                                  'TOSL', 'GA', 'SA', 'SV','SV_Pct', 'GAA', 'SO', 'GPS', 'MIN', 'QS', 'QS_Pct', 'RBS', 'GA_Pct_Rel', 'GSAA',
                                  'G', 'A', 'PTS', 'PIM', 'Player_URL')

      #Add Pos 'G'
      gbs_df <- gbs_df %>%
        mutate(
          Pos1 = 'G'
        ) %>%
        relocate(
          Pos1, .before = GP
        )

      #Edit table to convert player url into player id, relocate next to player name, and remove rank col
      gbs_df <- gbs_df %>%
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
        )

      # Replace Asterisk from HoF Players with Separate Column
      gbs_df <- gbs_df %>%
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


    # REG GSO -----------------------------------------------------------------

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

        # Length Variables (used for status message)
        gso_num <- 0

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

          # Print Message to Update User
          gso_num <- gso_num + 1
          flush.console()
          cat(
            paste0(
              strrep(bgGreen(green("=")), 50*((gbs_len+gso_num)/tot_len)),
              strrep(bgWhite(" "), 50-(50*((gbs_len+gso_num)/tot_len))),
              " ",
              round(((gbs_len+gso_num)/tot_len)*100,digits=0), "%"
            ),
            " \r"
          )

        }

        #Set colnames for table
        colnames(gso_df)[1:8] <- c('Season', 'Player_Name', 'Team', 'soAtt', 'soMade', 'soMiss', 'soPct', 'Player_URL')

        #Edit table to convert player url into player id, relocate next to player name, and remove rank col
        gso_df <- gso_df %>%
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
            -Player_URL
          )

        # Replace Asterisk from HoF Players with Separate Column
        gso_df <- gso_df %>%
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

      } else{
        #skip
      }


    # REG TBL COMBINE ---------------------------------------------------------

      goalie_reg_stats <- gbs_df

      if (length(gso_yrs) != 0) {

        #Add shoout table to basic stats
        goalie_reg_stats <- goalie_reg_stats %>%
          left_join(
            gso_df[, c('Season', 'Player_ID', 'Team', 'soAtt', 'soMade', 'soMiss', 'soPct')],
            by = c('Season', 'Player_ID', 'Team')
          )

      } else{
        #skip
      }

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

    goalie_reg_stats <- data.frame()

  }


  # PLAYOFFS ----------------------------------------------------------------

  if(post == TRUE){

      # POST GBS ----------------------------------------------------------------

      #Create table url variables
      gbps_urlstrt <- 'https://www.hockey-reference.com/playoffs/NHL_'
      gbps_urlend <- '_goalies.html'

      #Create xpath variables to get player id
      gbps_xpath_strt <- '//*[@id="stats"]/tbody/tr['
      gbps_xpath_mid <- ']/td['
      gbps_xpath_end <- ']/a'

      #Create empty dataframe to fill
      gbps_df <- setNames(data.frame(matrix(ncol = 27, nrow = 0)),
                          c("Season", "Rk", "Player", "Age", "Tm", "GP", "GS", "W", "L",
                            "GA", "SA", "SV", "SV%", "GAA", "SO", "GPS", "MIN", "QS", "QS%",
                            "RBS", "GA%-", "GSAA", "G", "A", "PTS", "PIM"))
      gbps_id_df <- data.frame()

      # Length Variables (used for status message)
      tot_len <- length(gbps_yrs)
      gbps_len <- length(gbps_yrs)
      gbps_num <- 0

      # First Update Message
      tot_strt_time <- tic()
      if(reg == TRUE & post == TRUE){
        message("Note: regular season and playoff stats will be downloaded separatley before being combined and returned as one output")
      }
      message("Downloading NHL goalie playoff stats for selected seasons, completion status: ")
      cat(
        paste0(strrep(bgWhite(" "), 49), " ", 0, "%"),
        " \r"
      )

      #Scrape data
      for (i in gbps_yrs){

        #Create url to scrape
        gbps_url <- read_html(paste(gbps_urlstrt, i, gbps_urlend, sep=""))

        #Get table
        gbps_df_temp <- gbps_url %>%
          html_node('table#stats') %>%    # select the desired table
          html_table()

        #Edit colnames
        names(gbps_df_temp) <- as.matrix(gbps_df_temp[1, ])
        gbps_df_temp <- gbps_df_temp[-1, ]
        gbps_df_temp <- cbind(Season = i, gbps_df_temp)

        #Get number of goalies in table for next loop
        rows <- (1:nrow(gbps_df_temp))

        #Loop through each row of table to get player url (to be used for player id)
        for (l in rows) {

          #Get url (player id)
          gbps_id_df_temp <- gbps_url %>%
            html_node(
              xpath = paste(gbps_xpath_strt, l, gbps_xpath_mid, 1, gbps_xpath_end, sep = "")
            ) %>%
            html_attr('href')

          #Turn into df to combine with stats table
          gbps_id_df_temp <- data.frame(gbps_id_df_temp)
          gbps_id_df <- rbind(gbps_id_df, gbps_id_df_temp)

        }

        #Combine data
        gbps_df_temp <- cbind(gbps_df_temp, gbps_id_df)

        #Find rows of goalies that have multiple listings based on playing for more than one team in season
        gbps_tots <- gbps_df_temp %>%
          select(
            Season, gbps_id_df_temp, Tm
          ) %>%
          subset(
            Tm == 'TOT'
          )

        #Remove blank rows
        gbps_df_temp <- gbps_df_temp[!(gbps_df_temp$Rk=="Rk"),]

        #Add to full dataframe
        gbps_df <- rbind(gbps_df, gbps_df_temp)

        #Clear temporary df's
        gbps_df_temp <- NULL
        gbps_id_df <- NULL

        # Print Message to Update User
        gbps_num <- gbps_num + 1
        flush.console()
        cat(
          paste0(
            strrep(bgGreen(green("=")), 50*(gbps_num/tot_len)),
            strrep(bgWhite(" "), 50-(50*(gbps_num/tot_len))),
            " ",
            round((gbps_num/tot_len)*100,digits=0), "%"
          ),
          " \r"
        )

      }

      #Set colnames for table
      colnames(gbps_df)[1:27] <- c('Season', 'Rk', 'Player_Name', 'Age', 'Team', 'GP', 'GS', 'W', 'L', 'GA', 'SA', 'SV','SV_Pct', 'GAA',
                                   'SO', 'GPS', 'MIN', 'QS', 'QS_Pct', 'RBS', 'GA_Pct_Rel', 'GSAA', 'G', 'A', 'PTS', 'PIM', 'Player_URL')

      #Add Pos 'G'
      gbps_df <- gbps_df %>%
        mutate(
          Pos1 = 'G'
        ) %>%
        relocate(
          Pos1, .before = GP
        )

      #Edit table to convert player url into player id, relocate next to player name, and remove rank col
      gbps_df <- gbps_df %>%
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
        )

      # Replace Asterisk from HoF Players with Separate Column
      gbps_df <- gbps_df %>%
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

    gbps_df <- data.frame()

  }


  # RETURN OUTPUT -----------------------------------------------------------

    # Combine with Postseason Dataframe
    goalie_stats <- rbind.fill(goalie_reg_stats, gbps_df)

    # Return Full Output
    return(goalie_stats)

} #end function

