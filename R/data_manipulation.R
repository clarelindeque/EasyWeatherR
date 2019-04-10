library(tidyverse)
library(janitor)

#' Consolidate weather data
#'
#' Combine weather data from different years into a tbl, and optionally
#' write it out as a CSV file.
#'
#' @param years A numeric array
#' @param write_file A boolean to determine whether to write the merged data
#' out to a file
#' @param filename A text string specifying the filename to write the data
#' structure out to
#' @return A data frame containing weather data from the specified years.
#' (Also writes out a csv of the data - see write_file and filename
#' parameters above)
#' @examples
#' years <- c(2018,2019)
#' merge_weather_csv(years, TRUE, "my_data.csv")
#' merge_weather_csv(years)

merge_weather_csv <- function(years,
                              write_file=FALSE,
                              filename="all_data.csv") {
  all_data <- NULL

  for (y in years) {
    print(y)
    data <- read_csv(paste(y, ".csv", sep="")) %>%
      clean_names() %>% # clean up column names
      rename_all(list(~str_replace(., "u_00b0", "deg")))  # replace ascii w "deg"

    # missing values show as "--" or "--.-" - replace them all with NA
    data <- data %>%
      replace(. == "--", NA) %>%
      replace(. == "--.-", NA)

    # Split "time" column into "date" and "time"
    data <- data %>%
      mutate(time = as.POSIXct(time, format="%d/%m/%Y %H:%M")) %>%  # format time
      mutate(date = as.Date(time, format="%d/%m/%Y")) %>%  # make a date column
      select(no, date, time, everything()) # reorder columns
    all_data <- bind_rows(all_data, data)
  }

  # make character columns numeric
  all_data <- all_data %>%
    mutate_if(is.character, as.numeric)

  if (write_file) {
    write_csv(all_data, filename)
  }

  return(all_data)
}

#' Add a column indicating season
#'
#' Adds a column to the weather data frame indicating season as a factor, with levels
#' Summer, Autumn, Winter, Spring. The default input values are for the southern
#' hemisphere, Cape Town specifically.
#'
#' @param data A data frame containing weather data
#' @param date_col A string specifying the name of the column containing the dates of
#' the observations
#' @param month_season_begin A four element numeric array specifying the months in which
#' each season begins
#' @param day_season_begin An integer OR a four element numeric array specifying the day of
#' the month on which each season begins
#' @param drop_date_split Boolean to determine whether to drop the numeric day, month, year
#' columns which this function adds during processing, before returning the data frame

add_season <- function(data,
                       date_col="date",
                       month_season_begin = c(11, 2, 5, 8),
                       day_season_begin = 15,
                       drop_date_split = TRUE) {

  # Some error checking
  if (length(month_season_begin) != 4) {
    error("The month_season_begin input parameter must be a four-element numeric vector")
  }

  if (length(day_season_begin) != 4) {
    if (length(day_season_begin) == 1) {
      day_season_begin <- rep(day_season_begin, 4)
    } else {
      error("The day_season_begin input parameter must either be a four-element numeric vector, or a single integer")
    }
  }

  # Utility variables
  seasons <- c("Summer", "Autumn", "Winter", "Spring")

  # Split the date into day month year as integers
  data <- data %>%
    split_date()

  # Now add a season column - start with whichever season (summer or winter)
  # straddles year-end
  max_month  <- max(month_season_begin)
  max_month_index <- match(max_month, month_season_begin)
  straddle_season <- seasons[max_month_index]

  # First define the column filled only with the season that straddles year end
  # to save us messy logic later on
  data <- data %>%
    mutate(season = straddle_season)

  for (k in seq(from=1, to=4, by=1)) {

    this_season <- seasons[k]
    start_month <- month_season_begin[k] # the month that this season starts in
    end_month <- ifelse(k < 4, month_season_begin[k+1], month_season_begin[1])
    start_month_day <- day_season_begin[k]
    end_month_day <- ifelse(k < 4, day_season_begin[k+1] - 1, day_season_begin[1] - 1)

    if (start_month != max_month) {

      data <- data %>%
        # do the intervening months first (full months)
        mutate(season = ifelse(month > start_month & month < end_month, this_season, season)) %>%
        # then do the days in the month in which the season starts
        mutate(season = ifelse(month == start_month & day  >= start_month_day, this_season, season)) %>%
        # then do the days in the month in which the season ends
        mutate(season = ifelse(month == end_month & day  < end_month_day, this_season, season))

    } else {
      next # we've already done this season (it's the straddle one)
    }
  }

  # Delete date split columns (this is cumbersome)
  if (drop_date_split) {
    data <- data %>%
      select(-day, -month, -year)
  }

  # Make seasons a factor and order it correctly for future plotting aesthetics
  data <- data %>%
    mutate(season = factor(season, levels=seasons))
}

#' Add numeric columns indicating day, month and year
#'
#' Adds a columns to the weather data frame indicating day of the month, month and
#' year as integer
#'
#' @param data A data frame containing weather data
#' @param date_col A string specifying the name of the column containing the dates of
#' the observations

split_date <- function(data,
                       date_col="date") {

  # Check we don't have them already


  data <- data %>%
    separate(col=date_col, into=c("year", "month", "day"), remove=FALSE) %>%
    mutate(day = as.integer(day)) %>%
    mutate(month = as.integer(month)) %>%
    mutate(year = as.integer(year))
}
