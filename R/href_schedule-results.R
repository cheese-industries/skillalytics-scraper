
# DETAILS -----------------------------------------------------------------

#' Scrape NHL Game Results from hockey-reference.com
#'
#' Gathers year-by-year game results from each NHL season selected and combines results into one dataset.
#' @param seas Which seasons would you like to retrieve data for? No default set.
#' @param reg Include regular season data? Defaults to TRUE
#' @param post Include postseason data? Defaults to TRUE
#' @keywords skillalytics
#' @export
#' @examples
#' href_results()


# HREF GAME RESULTS -------------------------------------------------------

href_results <- function(seas, reg = TRUE, post = TRUE){

  # SETUP -------------------------------------------------------------------

    # Initialize Years based on Available Data
    scrape_setup <- skillalytics_setup(seas)
    res_yrs <- scrape_setup$res_yrs
    resp_yrs <- scrape_setup$resp_yrs
    tm_ref <- scrape_setup$tm_ref


  # REGULAR SEASON ----------------------------------------------------------

  if(reg == TRUE){

    res_urlstrt <- 'https://www.hockey-reference.com/leagues/NHL_'
    res_urlend <- '_games.html'

    res_df <- data.frame()

    # Number of Seasons to Scrape (used for status message)
    seas_len <- length(res_yrs)
    seas_num <- 0

    # First Update Message
    tot_strt_time <- tic()
    if(reg == TRUE & post == TRUE){
    message("Note: regular season and playoff results will be downloaded separatley before being combined and returned as one output")
    }
    message("Downloading NHL regular season game results for selected seasons, completion status: ")
    cat(
      paste0(strrep(bgWhite(" "), 49), " ", 0, "%"),
      " \r"
    )

    for (i in res_yrs){

      res_url <- read_html(paste(res_urlstrt, i, res_urlend, sep=""))

      res_df_temp <- res_url %>%
        html_node('table#games') %>%    # select the desired table
        html_table()

      #Add cols
      res_df_temp <- cbind(Season = i, 'Game Type' = 'REG', res_df_temp)

      res_df <- rbind(res_df, res_df_temp)
      res_df_temp <- NULL

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

    colnames(res_df) <- c('Season', 'Game_Type', 'Date', 'Away_Team_Name', 'Away_Score', 'Home_Team_Name', 'Home_Score',
                          'Ended', 'Attendance', 'Game_Length', 'Game_Notes')

    # Add Team ID from Reference List
    res_df <- res_df %>%
      # Get Away Team ID
      left_join(
        tm_ref,
        by = c('Away_Team_Name' = 'Team_Name')
      ) %>%
      rename(
        Away_Team_ID = Team_ID
      ) %>%
      relocate(
        Away_Team_ID,
        .before = Away_Team_Name
      ) %>%
      # Get Home Team ID
      left_join(
        tm_ref,
        by = c('Home_Team_Name' = 'Team_Name')
      ) %>%
      rename(
        Home_Team_ID = Team_ID
      ) %>%
      relocate(
        Home_Team_ID,
        .before = Home_Team_Name
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

  # If reg = FALSE
  } else {

    res_df <- data.frame()

  }


  # PLAYOFFS ----------------------------------------------------------------

  if(post == TRUE){

    resp_urlstrt <- 'https://www.hockey-reference.com/leagues/NHL_'
    resp_urlend <- '_games.html'

    resp_df <- data.frame()

    # Number of Seasons to Scrape (used for status message)
    seas_len <- length(resp_yrs)
    seas_num <- 0

    # First Update Message
    tot_strt_time <- tic()
    message(
      paste0(
        "Downloading NHL playoff game results for selected seasons, completion status: "
      )
    )
    cat(
      paste0(strrep(bgWhite(" "), 49), " ", 0, "%"),
      " \r"
    )

    for (i in resp_yrs){

      resp_url <- read_html(paste(resp_urlstrt, i, resp_urlend, sep=""))

      resp_df_temp <- resp_url %>%
        html_node('table#games_playoffs') %>%    # select the desired table
        html_table()

      #Add cols
      resp_df_temp <- cbind(Season = i, 'Game Type' = 'POST', resp_df_temp)

      resp_df <- rbind(resp_df, resp_df_temp)
      resp_df_temp <- NULL

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

    colnames(resp_df) <- c('Season', 'Game_Type', 'Date', 'Away_Team_Name', 'Away_Score', 'Home_Team_Name', 'Home_Score',
                           'Ended', 'Attendance', 'Game_Length', 'Game_Notes')

    # Add Team ID from Reference List
    resp_df <- resp_df %>%
      # Get Away Team ID
      left_join(
        tm_ref,
        by = c('Away_Team_Name' = 'Team_Name')
      ) %>%
      rename(
        Away_Team_ID = Team_ID
      ) %>%
      relocate(
        Away_Team_ID,
        .before = Away_Team_Name
      ) %>%
      # Get Home Team ID
      left_join(
        tm_ref,
        by = c('Home_Team_Name' = 'Team_Name')
      ) %>%
      rename(
        Home_Team_ID = Team_ID
      ) %>%
      relocate(
        Home_Team_ID,
        .before = Home_Team_Name
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

  # If post = FALSE
  } else {

    resp_df <- data.frame()

  }


  # RETURN OUTPUT -----------------------------------------------------------

    # Combine with Postseason Dataframe
    full_res <- rbind.fill(res_df, resp_df)

    # Return Full Output
    return(full_res)

} #end function
