
# DETAILS -----------------------------------------------------------------

#' Scrape NHL Standings from hockey-reference.com
#'
#' Gathers year-by-year standings from each NHL season selected and combines results into one dataset.
#' @param seas Which seasons would you like to retrieve data for? No default set.
#' @keywords skillalytics
#' @export
#' @examples
#' href_standings()


# HREF STANDINGS ----------------------------------------------------------

href_standings <- function(seas){

  # SETUP -------------------------------------------------------------------

    # Initialize Years based on Available Data
    scrape_setup <- skillalytics_setup(seas)
    tbl_yrs <- scrape_setup$tbl_yrs
    tm_ref <- scrape_setup$tm_ref


  # LEAGUE STANDINGS TABLE --------------------------------------------------

    #Create table url variables
    st_urlstrt <- 'https://www.hockey-reference.com/leagues/NHL_'
    st_urlend <- '_standings.html'

    # no conf from 1918-1974 (#standings)
    # clarence campbell and prince of wales conf from 1975-1993 (#standings_CAM; #standings_WAL)
    # east and west conf from 1994+ (#standings_EAS; #standings_WES)

    #Create empty dataframe to fill
    tbl_df <- setNames(data.frame(matrix(ncol = 21, nrow = 0)),
                      c("Season", "Half", "Conference", "Division", "Team", "GP", "W", "L", "OL", "T",
                        "PTS", "PTS%", "GF", "GA", "SRS", "SOS", "RPt%", "ROW", "RW", "RgRec", "RgPt%"))
    tbl_df$Conference <- as.character(tbl_df$Conference)

    # Number of Seasons to Scrape (used for status message)
    seas_len <- length(tbl_yrs)
    seas_num <- 0

    # First Update Message
    tot_strt_time <- tic()
    message(
      paste0(
        "Downloading NHL standings for selected seasons, completion status: "
      )
    )
    cat(
      paste0(strrep(bgWhite(" "), 49), " ", 0, "%"),
      " \r"
    )

    #Scrape data
    for (i in tbl_yrs){

      #Create url to scrape
      st_url <- read_html(paste(st_urlstrt, i, st_urlend, sep=""))

      #If stmt to deterine how to scrape & build table based on season
      #Pre 1922 had First and Second half seasons
      if (i <= 1921) {

        #Get table
        tbl_df_temp <- st_url %>%
          html_node('table#standings') %>%
          html_table()
        tbl_df_temp <- cbind(Season = i, tbl_df_temp)

        #Add col for season Half and rename Team col
        tbl_df_temp <- tbl_df_temp %>%
          mutate(
            rownum = as.numeric(row.names(tbl_df_temp)),
            Half = ifelse(rownum < which(grepl('2nd Half', get('1st Half'))), 'First', 'Second')
          ) %>%
          select(
            -rownum
          ) %>%
          dplyr::rename(
            Team = '1st Half'
          )

        #Remove blank rows
        tbl_df_temp <- tbl_df_temp[!(tbl_df_temp$Team == '2nd Half'),]

        #1922 to 1926 no longer First and Second half seasons, no divisions
      } else if (i > 1921 & i <= 1926) {

        #Get table
        tbl_df_temp <- st_url %>%
          html_node('table#standings') %>%
          html_table()

        #Edit colnames
        tbl_df_temp <- cbind(Season = i, tbl_df_temp)
        colnames(tbl_df_temp)[2] <- 'Team'

        #1927 to 1938 divisions added
      } else if (i > 1926 & i <= 1938) {

        #Get table
        tbl_df_temp <- st_url %>%
          html_node('table#standings') %>%
          html_table()

        #Edit colnames
        tbl_df_temp <- cbind(Season = i, tbl_df_temp)
        colnames(tbl_df_temp)[2] <- 'Team'

        #Add col for division
        tbl_df_temp <- tbl_df_temp %>%
          mutate(
            rownum = as.numeric(row.names(tbl_df_temp)),
            Division = gsub(" Division", "",
                            ifelse(rownum <= which(grepl('* Division', Team[2:nrow(tbl_df_temp)])), Team[1],
                                   Team[which(grepl('* Division', Team[2:nrow(tbl_df_temp)]))+1])
            )
          ) %>%
          select(
            -rownum
          ) %>%
          relocate(
            Division, .before = Team
          )

        #Remove blank rows
        tbl_df_temp <- tbl_df_temp[!grepl('* Division', tbl_df_temp$Team),]

        #1939 to 1967 divisions removed
      } else if (i > 1938 & i <= 1967) {

        #Get table
        tbl_df_temp <- st_url %>%
          html_node('table#standings') %>%
          html_table()

        #Edit colnames
        tbl_df_temp <- cbind(Season = i, tbl_df_temp)
        colnames(tbl_df_temp)[2] <- 'Team'

        #1967 to 1974 split into East and West divisions
      } else if (i > 1967 & i <= 1974) {

        #Get table
        tbl_df_temp <- st_url %>%
          html_node('table#standings') %>%
          html_table()

        #Edit colnames
        tbl_df_temp <- cbind(Season = i, tbl_df_temp)
        colnames(tbl_df_temp)[2] <- 'Team'

        #Add col for division
        tbl_df_temp <- tbl_df_temp %>%
          mutate(
            rownum = as.numeric(row.names(tbl_df_temp)),
            Division = gsub(" Division", "",
                            ifelse(rownum <= which(grepl('* Division', Team[2:nrow(tbl_df_temp)])), Team[1],
                                   Team[which(grepl('* Division', Team[2:nrow(tbl_df_temp)]))+1])
            )
          ) %>%
          select(
            -rownum
          ) %>%
          relocate(
            Division, .before = Team
          )

        #Remove blank rows
        tbl_df_temp <- tbl_df_temp[!grepl('* Division', tbl_df_temp$Team),]

        #1974 to 1993 split into Clarence Campbell and Prince of Wales conferences
      } else if (i > 1974 & i <= 1993) {

        #Get Clarence Campbell table
        tbl_df_cam_temp <- st_url %>%
          html_node('table#standings_CAM') %>%
          html_table()

        #Edit cols
        tbl_df_cam_temp <- cbind(Season = i, Conference = as.character('Clarence Campbell'), tbl_df_cam_temp)
        colnames(tbl_df_cam_temp)[3] <- 'Team'

        #Add col for division
        tbl_df_cam_temp <- tbl_df_cam_temp %>%
          mutate(
            rownum = as.numeric(row.names(tbl_df_cam_temp)),
            Division = gsub(" Division", "",
                            ifelse(rownum <= which(grepl('* Division', Team[2:nrow(tbl_df_cam_temp)])), Team[1],
                                   Team[which(grepl('* Division', Team[2:nrow(tbl_df_cam_temp)]))+1])
            )
          ) %>%
          select(
            -rownum
          ) %>%
          relocate(
            Division, .before = Team
          )

        #Remove blank rows
        tbl_df_cam_temp <- tbl_df_cam_temp[!grepl('* Division', tbl_df_cam_temp$Team),]

        #Get Prince of Wales table
        tbl_df_wal_temp <- st_url %>%
          html_node('table#standings_WAL') %>%
          html_table()

        #Edit cols
        tbl_df_wal_temp <- cbind(Season = i, Conference = as.character('Prince of Wales'), tbl_df_wal_temp)
        colnames(tbl_df_wal_temp)[3] <- 'Team'

        #Add col for division
        tbl_df_wal_temp <- tbl_df_wal_temp %>%
          mutate(
            rownum = as.numeric(row.names(tbl_df_wal_temp)),
            Division = gsub(" Division", "",
                            ifelse(rownum <= which(grepl('* Division', Team[2:nrow(tbl_df_wal_temp)])), Team[1],
                                   Team[which(grepl('* Division', Team[2:nrow(tbl_df_wal_temp)]))+1])
            )
          ) %>%
          select(
            -rownum
          ) %>%
          relocate(
            Division, .before = Team
          )

        #Remove blank rows
        tbl_df_wal_temp <- tbl_df_wal_temp[!grepl('* Division', tbl_df_wal_temp$Team),]

        #Combine conference tables
        tbl_df_temp <- rbind(tbl_df_cam_temp, tbl_df_wal_temp)

        #1994 conference names changed to Eastern and Western conferences
      } else if (i > 1993 & i <= 1998) {

        #Get Eastern table
        tbl_df_eas_temp <- st_url %>%
          html_node('table#standings_EAS') %>%
          html_table()

        #Edit cols
        tbl_df_eas_temp <- cbind(Season = i, Conference = as.character('Eastern'), tbl_df_eas_temp)
        colnames(tbl_df_eas_temp)[3] <- 'Team'

        #Add col for division
        tbl_df_eas_temp <- tbl_df_eas_temp %>%
          mutate(
            rownum = as.numeric(row.names(tbl_df_eas_temp)),
            Division = gsub(" Division", "",
                            ifelse(rownum <= which(grepl('* Division', Team[2:nrow(tbl_df_eas_temp)])), Team[1],
                                   Team[which(grepl('* Division', Team[2:nrow(tbl_df_eas_temp)]))+1])
            )
          ) %>%
          select(
            -rownum
          ) %>%
          relocate(
            Division, .before = Team
          )

        #Remove blank rows
        tbl_df_eas_temp <- tbl_df_eas_temp[!grepl('* Division', tbl_df_eas_temp$Team),]

        #Get Western table
        tbl_df_wes_temp <- st_url %>%
          html_node('table#standings_WES') %>%
          html_table()

        #Edit cols
        tbl_df_wes_temp <- cbind(Season = i, Conference = as.character('Western'), tbl_df_wes_temp)
        colnames(tbl_df_wes_temp)[3] <- 'Team'

        #Add col for division
        tbl_df_wes_temp <- tbl_df_wes_temp %>%
          mutate(
            rownum = as.numeric(row.names(tbl_df_wes_temp)),
            Division = gsub(" Division", "",
                            ifelse(rownum <= which(grepl('* Division', Team[2:nrow(tbl_df_wes_temp)])), Team[1],
                                   Team[which(grepl('* Division', Team[2:nrow(tbl_df_wes_temp)]))+1])
            )
          ) %>%
          select(
            -rownum
          ) %>%
          relocate(
            Division, .before = Team
          )

        #Remove blank rows
        tbl_df_wes_temp <- tbl_df_wes_temp[!grepl('* Division', tbl_df_wes_temp$Team),]

        #Combine conference tables
        tbl_df_temp <- rbind(tbl_df_eas_temp, tbl_df_wes_temp)

        #1994 to 2013 conference names changed to Eastern and Western conferences with 3 divisions each
      } else if (i > 1998 & i <= 2013) {

        #Get Eastern table
        tbl_df_eas_temp <- st_url %>%
          html_node('table#standings_EAS') %>%
          html_table()

        #Edit cols
        tbl_df_eas_temp <- cbind(Season = i, Conference = 'Eastern', tbl_df_eas_temp)
        colnames(tbl_df_eas_temp)[3] <- 'Team'

        #Add col for division
        tbl_df_eas_temp <- tbl_df_eas_temp %>%
          mutate(
            rownum = as.numeric(row.names(tbl_df_eas_temp)),
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
        tbl_df_eas_temp <- tbl_df_eas_temp[!grepl('* Division', tbl_df_eas_temp$Team),]

        #Get Western table
        tbl_df_wes_temp <- st_url %>%
          html_node('table#standings_WES') %>%
          html_table()

        #Edit cols
        tbl_df_wes_temp <- cbind(Season = i, Conference = 'Western', tbl_df_wes_temp)
        colnames(tbl_df_wes_temp)[3] <- 'Team'

        #Add col for division
        tbl_df_wes_temp <- tbl_df_wes_temp %>%
          mutate(
            rownum = as.numeric(row.names(tbl_df_wes_temp)),
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
        tbl_df_wes_temp <- tbl_df_wes_temp[!grepl('* Division', tbl_df_wes_temp$Team),]

        #Combine conference tables
        tbl_df_temp <- rbind(tbl_df_eas_temp, tbl_df_wes_temp)

        #2013 to 2020 changed to 2 divisions in each conference
      } else if (i > 2013 & i <= 2020) {

        #Get Eastern table
        tbl_df_eas_temp <- st_url %>%
          html_node('table#standings_EAS') %>%
          html_table()

        #Edit cols
        tbl_df_eas_temp <- cbind(Season = i, Conference = as.character('Eastern'), tbl_df_eas_temp)
        colnames(tbl_df_eas_temp)[3] <- 'Team'

        #Add col for division
        tbl_df_eas_temp <- tbl_df_eas_temp %>%
          mutate(
            rownum = as.numeric(row.names(tbl_df_eas_temp)),
            Division = gsub(" Division", "",
                            ifelse(rownum <= which(grepl('* Division', Team[2:nrow(tbl_df_eas_temp)])), Team[1],
                                   Team[which(grepl('* Division', Team[2:nrow(tbl_df_eas_temp)]))+1])
            )
          ) %>%
          select(
            -rownum
          ) %>%
          relocate(
            Division, .before = Team
          )

        #Remove blank rows
        tbl_df_eas_temp <- tbl_df_eas_temp[!grepl('* Division', tbl_df_eas_temp$Team),]

        #Get Western table
        tbl_df_wes_temp <- st_url %>%
          html_node('table#standings_WES') %>%
          html_table()

        #Edit cols
        tbl_df_wes_temp <- cbind(Season = i, Conference = as.character('Western'), tbl_df_wes_temp)
        colnames(tbl_df_wes_temp)[3] <- 'Team'

        #Add col for division
        tbl_df_wes_temp <- tbl_df_wes_temp %>%
          mutate(
            rownum = as.numeric(row.names(tbl_df_wes_temp)),
            Division = gsub(" Division", "",
                            ifelse(rownum <= which(grepl('* Division', Team[2:nrow(tbl_df_wes_temp)])), Team[1],
                                   Team[which(grepl('* Division', Team[2:nrow(tbl_df_wes_temp)]))+1])
            )
          ) %>%
          select(
            -rownum
          ) %>%
          relocate(
            Division, .before = Team
          )

        #Remove blank rows
        tbl_df_wes_temp <- tbl_df_wes_temp[!grepl('* Division', tbl_df_wes_temp$Team),]

        #Combine conference tables
        tbl_df_temp <- rbind(tbl_df_eas_temp, tbl_df_wes_temp)

        #2021 no conferences, four divisions
      } else if (i > 2020) {

        #Get table
        tbl_df_temp <- st_url %>%
          html_node('table#standings') %>%
          html_table()

        #Edit cols
        tbl_df_temp <- cbind(Season = i, tbl_df_temp)
        colnames(tbl_df_temp)[2] <- 'Team'

        #Add col for division
        tbl_df_temp <- tbl_df_temp %>%
          mutate(
            rownum = as.numeric(row.names(tbl_df_temp)),
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
        tbl_df_temp <- tbl_df_temp[!grepl('* Division', tbl_df_temp$Team),]

      }

      tbl_df  <- plyr::rbind.fill(tbl_df, tbl_df_temp)
      tbl_df_temp <- NULL

      # Print Message to Update User
      seas_num <- seas_num + 1
      flush.console()
      cat(
        paste0(
          strrep(bgGreen(green("=")), 50*(seas_num/seas_len)),
          strrep(bgWhite(" "), 50-(50*(seas_num/seas_len))),
          " ",
          round((seas_num/seas_len)*100,digits=0), "%"
        ),
        " \r"
      )

    }

    #Set colnames for table
    colnames(tbl_df)[1:21] <- c("Season", "Half", "Conference", "Division", "Team", "GP", "W", "L", "OL", "T",
                               "PTS", "PTS_Pct", "GF", "GA", "SRS", "SOS", "RPt_Pct", "ROW", "RW", "RgRec", "RgPt_Pct")

    # Add Playoff Column and Edit Team Name
    tbl_df <- tbl_df %>%
      mutate(
        Playoffs = ifelse(grepl('\\*', tbl_df$Team) == TRUE, TRUE, FALSE),
        Team_Name = gsub("\\*", "", Team)
      ) %>%
      relocate(
        Team_Name, .after = Team
      ) %>%
      relocate(
        Playoffs, .after = Team_Name
      ) %>%
      select(
        -Team
      )

    # Add Team ID from Reference List
    tbl_df <- tbl_df %>%
      left_join(
        tm_ref,
        by = 'Team_Name'
      ) %>%
      relocate(
        Team_ID,
        .before = Team_Name
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
      sprintf("%2i", minute(seconds_to_period(tot_time_diff))),
      "min",
      round(tot_time_diff%%60,0),
      "sec",
      " \n"
    )

  # RETURN OUTPUT -----------------------------------------------------------

    return(tbl_df)

} #end function

