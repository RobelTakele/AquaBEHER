test_that("calcSeasCal handles missing columns", {
  data <- data.frame(
    Month = 1:10, Day = 1:10, R = runif(10),
    AVAIL = runif(10)
  )
  expect_error(
    calcSeasCal(data, "01-01", "12-31", "12-31", 100),
    "The required data columns Year are missing!"
  )

  data <- data.frame(
    Year = 1982:1991, Month = 1:10, Day = 1:10,
    AVAIL = runif(10)
  )
  expect_error(
    calcSeasCal(data, "01-01", "12-31", "12-31", 100),
    "The required data columns R are missing!"
  )
})

test_that("calcSeasCal handles invalid date format", {
  data <- data.frame(
    Year = 1982:1991, Month = 1:10, Day = 1:10,
    R = runif(10), AVAIL = runif(10)
  )

  expect_error(calcSeasCal(data, "01-1", "12-31", "12-31", 100),
    "The start date for the onset window 'onsetWind.start' is
         missing or not in 'MM-DD' format!",
    fixed = TRUE
  )

  expect_error(calcSeasCal(data, "01-01", "12-3", "12-31", 100),
    "The end date for the onset window 'onsetWind.end' is
         missing or not in 'MM-DD' format!",
    fixed = TRUE
  )
})

test_that("calcSeasCal handles invalid soilWHC values", {
  data <- data.frame(
    Year = 1982:1991, Month = 1:10,
    Day = 1:10, R = runif(10), AVAIL = runif(10)
  )

  expect_error(calcSeasCal(data, "01-01", "12-31", "12-31", "invalid"),
    "The soil water holding capacity 'soilWHC' must be a
         positive number!",
    fixed = TRUE
  )

  expect_error(calcSeasCal(data, "01-01", "12-31", "12-31", -50),
    "The soil water holding capacity 'soilWHC' must be a
         positive number!",
    fixed = TRUE
  )
})

test_that("calcSeasCal handles invalid R and AVAIL values", {
  data <- data.frame(
    Year = 1982:1991, Month = 1:10, Day = 1:10,
    R = c(-1, runif(9)), AVAIL = runif(10)
  )

  expect_error(calcSeasCal(data, "01-01", "12-31", "12-31", 100),
    "The actual-to-potential evapotranspiration ratio 'R'
         must be between 0 and 1!",
    fixed = TRUE
  )

  data$R <- runif(10)
  data$AVAIL <- c(-5, runif(9))
  expect_error(calcSeasCal(data, "01-01", "12-31", "12-31", 100),
    "The available soil water 'AVAIL' column contains invalid or
         negative values!",
    fixed = TRUE
  )
})

test_that("calcSeasCal returns expected output structure", {
  data <- data.frame(
    Year = 1982, Month = 1, Day = 1,
    R = runif(365), AVAIL = runif(365)
  )

  result <- calcSeasCal(data, "01-01", "12-31", "12-31", 100)

  expect_s3_class(result, "data.frame")
  expect_named(result, c(
    "Year", "OnsetDate", "OnsetDOY", "OnsetValue",
    "CessationDate", "CessationDOY", "CessationValue",
    "Duration"
  ))
})

###############################################################################
###############################################################################
#                >>>>>>>>>>   End of code   <<<<<<<<<<                        #
###############################################################################
