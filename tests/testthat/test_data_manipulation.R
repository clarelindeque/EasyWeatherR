# http://r-pkgs.had.co.nz/tests.html

context("Data manipulation")
library(EasyWeatherR)

test_that("incorrect number of rows or columns", {
  data <- merge_years(c(2018, 2019))
  expect_equal(nrow(data), 179)
  expect_equal(ncol(data), 23)
})

# omit heat index column from testing - in my files it has no values
# but other users may obtain numeric data

test_that("incorrect column format", {
  data <- merge_years(c(2018, 2019))
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
  expect_is(data$uv_u_w_c, "numeric")
  expect_is(data$uvi, "numeric")
})

test_that("incorrect class of output", {
  data <- merge_years(c(2018, 2019))
  expect_is(data, "tbl")
})
