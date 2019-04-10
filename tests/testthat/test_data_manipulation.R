# http://r-pkgs.had.co.nz/tests.html

context("Data manipulation")
library(EasyWeatherR)

test_that("incorrect number of rows or columns", {
  year1 <- read_csv(paste(2018, ".csv", sep=""))
  year2 <- read_csv(paste(2019, ".csv", sep=""))
  data <- merge_weather_csv(c(2018, 2019))
  expect_equal(nrow(data), nrow(year1) + nrow(year2))
  expect_equal(ncol(data), 23)
})

test_that("incorrect column format", {
  data <- merge_weather_csv(c(2018, 2019))
  expect_is(data$no, "numeric")
  expect_is(data$date, "Date")
  expect_is(data$time, "POSIXct")
  expect_is(data$indoor_temperature_deg_c, "numeric")
  expect_is(data$indoor_humidity_percent, "numeric")
  expect_is(data$outdoor_temperature_deg_c, "numeric")
  expect_is(data$outdoor_humidity_percent, "numeric")
  expect_is(data$wind_km_h, "numeric")
  expect_is(data$gust_km_h, "numeric")
  expect_is(data$dew_point_deg_c, "numeric")
  expect_is(data$wind_chill_deg_c, "numeric")
  expect_is(data$wind_direction_deg, "numeric")
  expect_is(data$abs_barometer_hpa, "numeric")
  expect_is(data$rel_barometer_hpa, "numeric")
  expect_is(data$rain_rate_mm_h, "numeric")
  expect_is(data$daily_rain_mm, "numeric")
  expect_is(data$monthly_rain_mm, "numeric")
  expect_is(data$yearly_rain_mm, "numeric")
  expect_is(data$solar_rad_w, "numeric")
  expect_is(data$heat_index_deg_c, "numeric")
  expect_is(data$uv_u_w_c, "numeric")
  expect_is(data$uvi, "numeric")
})

test_that("incorrect class of output", {
  data <- merge_weather_csv(c(2018, 2019))
  expect_is(data, "tbl")
})

test_that("incorrect number of columns", {
  data <- merge_weather_csv(c(2018, 2019))
  new_data <- add_season(data, drop_date_split=TRUE)
  expect_equal(ncol(new_data), ncol(data)+1)
  new_data <- add_season(data, drop_date_split=FALSE)
  expect_equal(ncol(new_data), ncol(data)+4)
})

test_that("error message expected", {
  data <- merge_weather_csv(c(2018, 2019))
  expect_error(add_season(data, month_season_begin = 5, drop_date_split=TRUE))
  expect_error(add_season(data, day_season_begin = c(21, 24), drop_date_split=TRUE))
})
