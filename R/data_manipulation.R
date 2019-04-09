library(tidyverse)
library(janitor)

#' Combine weather data from different years into a tbl, and optionally
#'  write it out as a CSV file.
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
#' merge_years(years, TRUE, "my_data.csv")
#' merge_years(years)

merge_years <- function(years, write_file=FALSE, filename="all_data.csv") {
  all_data <- NULL

  for (y in years) {
    print(y)
    data <- read_csv(paste(y, ".csv", sep="")) %>%
      clean_names() %>% # clean up column names
      rename_all(funs(str_replace(., "u_00b0", "deg")))  # replace ascii w "deg"

    # Split "time" column into "date" and "time"
    data <- data %>%
      mutate(time = as.POSIXct(time, format="%d/%m/%Y %H:%M")) %>%  # format time
      mutate(date = as.Date(time, format="%d/%m/%Y")) %>%  # make a date column
      select(no, date, time, everything()) # reorder columns
    all_data <- bind_rows(all_data, data)
  }

  if (write_file) {
    write.csv(all_data, filename)
  }

  return(all_data)
}
