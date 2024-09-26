test_that("calcWatBal throws errors for missing data", {
  ## ***** Define a mock data frame:

  data <- data.frame(
    Lat = 45,
    Lon = 12,
    Elev = 100,
    Year = 2023,
    Month = 9,
    Day = 6,
    Rain = c(10, 5, NA),
    Eto = c(3, 2, 1)
  )

  ## ***** Test for missing 'Rain':

  expect_error(
    calcWatBal(data, soilWHC = 100),
    "Missing values detected in 'Rain', 'Eto', or 'soilWHC'"
  )

  ## ***** Test for missing 'Eto':

  data$Eto <- NA
  expect_error(
    calcWatBal(data, soilWHC = 100),
    "Missing values detected in 'Rain', 'Eto', or 'soilWHC'"
  )

  ## ***** Test for missing 'soilWHC':

  expect_error(
    calcWatBal(data, soilWHC = NULL),
    "Required data missing for 'Rain', 'Eto', or 'soilWHC'"
  )
})

test_that("calcWatBal throws errors for unrealistic values", {
  data <- data.frame(
    Lat = 45,
    Lon = 12,
    Elev = 100,
    Year = 2023,
    Month = 9,
    Day = 6,
    Rain = c(10, 25, 10),
    Eto = c(5, 2, 1)
  )

  ## ***** Test for low soilWHC:

  expect_error(
    calcWatBal(data, soilWHC = 10),
    "The soil has very low water holding capacity"
  )

  ## ***** Test for high soilWHC:

  expect_warning(
    result <- calcWatBal(data, soilWHC = 350),
    "The soil water holding capacity exceeded
    realistic limits and has been set to the upper limit ."
  )

  expect_equal(
    result$warnings[["soilWHC"]],
    "The soil water holding capacity exceeded
    realistic limits and has been set to the upper limit (300 mm)."
  )

  ## ***** Test for excessive Rain values:

  data$Rain <- c(10, 2500, 10)

  expect_warning(
    result <- calcWatBal(data, soilWHC = 100),
    "Some 'Rain' values exceeded 2000 mm and were set to this limit."
  )

  expect_equal(
    result$warnings[["Rain"]],
    "Some 'Rain' values exceeded 2000 mm and were set to this limit."
  )

  ## ***** Test for excessive Eto values:

  data$Rain <- c(10, 250, 10)
  data$Eto <- c(25, 2, 1)

  expect_warning(
    result <- calcWatBal(data, soilWHC = 100),
    "Some 'Eto' values exceeded 20 mm/day and were set to this limit."
  )

  expect_equal(result$warnings[["Eto"]], "Some 'Eto' values exceeded 20 mm/day and were set to this limit.")
})

test_that("calcWatBal processes valid data correctly", {
  data <- data.frame(
    Lat = 45,
    Lon = 12,
    Elev = 100,
    Year = 2023,
    Month = 9,
    Day = 6,
    Rain = c(10, 15, 20),
    Eto = c(5, 5, 5)
  )

  result <- calcWatBal(data, soilWHC = 100)

  ## ***** Check that result is a list and dataframe:

  expect_type(result, "list")
  expect_s3_class(result$data, "data.frame")

  ## ***** Check that warnings list is empty:

  expect_equal(result$warnings, list())

  ## ***** Check that result contains the expected columns:

  expected_columns <- c(
    "Lat", "Lon", "Elev", "Year", "Month", "Day", "Rain",
    "Eto", "RUNOFF", "DRAIN", "TRAN", "AVAIL", "R"
  )

  expect_true(all(expected_columns %in% colnames(result$data)))
})

test_that("calcWatBal calculates runoff, drainage, transpiration, and
          available soil moisture correctly", {
  data <- data.frame(
    Lat = c(-15, -15, -15),
    Lon = c(38, 38, 38),
    Elev = c(100, 100, 100),
    Year = c(2023, 2023, 2023),
    Month = c(9, 9, 9),
    Day = c(1, 2, 3),
    Rain = c(10, 20, 15),
    Eto = c(5, 5, 5)
  )

  soilWHC <- 100

  expected_results <- data.frame(
    Year = c(2023, 2023, 2023),
    Month = c(9, 9, 9),
    Day = c(1, 2, 3),
    Rain = c(10, 20, 15),
    Eto = c(5, 5, 5),
    RUNOFF = c(0, 0.966, 0.152),
    DRAIN = c(0, 0, 0),
    TRAN = c(0, 1.347, 2.643),
    AVAIL = c(10, 27.687, 39.892),
    R = c(0, 0.269, 0.529)
  )

  result <- calcWatBal(data, soilWHC = soilWHC)

  expect_equal(result$data$RUNOFF, expected_results$RUNOFF, tolerance = 1e-3)
  expect_equal(result$data$DRAIN, expected_results$DRAIN, tolerance = 1e-3)
  expect_equal(result$data$TRAN, expected_results$TRAN, tolerance = 1e-3)
  expect_equal(result$data$AVAIL, expected_results$AVAIL, tolerance = 1e-3)
  expect_equal(result$data$R, expected_results$R, tolerance = 1e-3)

  expect_equal(result$data$R, round(result$data$R, 3))
  expect_equal(result$data$AVAIL, round(result$data$AVAIL, 3))
  expect_equal(result$data$TRAN, round(result$data$TRAN, 3))
  expect_equal(result$data$DRAIN, round(result$data$DRAIN, 3))
  expect_equal(result$data$RUNOFF, round(result$data$RUNOFF, 3))
})


test_that("calcWatBal handles boundary conditions correctly", {
  ## ***** Test with extreme values for Rain and Eto:

  data <- data.frame(
    Lat = c(-15, -15),
    Lon = c(32, 32),
    Elev = c(100, 100),
    Year = c(2023, 2023),
    Month = c(9, 9),
    Day = c(1, 2),
    Rain = c(0, 0), # Extreme case: no rain
    Eto = c(0, 0) # Extreme case: no evaporation
  )

  soilWHC <- 100

  expected_results <- data.frame(
    Year = c(2023, 2023),
    Month = c(9, 9),
    Day = c(1, 2),
    Rain = c(0, 0),
    Eto = c(0, 0),
    RUNOFF = c(0, 0),
    DRAIN = c(0, 0),
    TRAN = c(0, 0),
    AVAIL = c(0, 0),
    R = c(0, 0)
  )

  result <- calcWatBal(data, soilWHC = soilWHC)

  expect_equal(result$data$RUNOFF, expected_results$RUNOFF)
  expect_equal(result$data$DRAIN, expected_results$DRAIN)
  expect_equal(result$data$TRAN, expected_results$TRAN)
  expect_equal(result$data$AVAIL, expected_results$AVAIL)
  expect_equal(result$data$R, expected_results$R)
})

test_that("calcWatBal handles small rainfall values correctly", {
  data <- data.frame(
    Lat = c(-15, -15),
    Lon = c(32, 32),
    Elev = c(100, 100),
    Year = c(2023, 2023),
    Month = c(9, 9),
    Day = c(1, 2),
    Rain = c(1, 1), ##  Small rainfall values
    Eto = c(5, 5)
  )

  soilWHC <- 100

  ## ***** Expected results with small rainfall values:

  expected_results <- data.frame(
    Year = c(2023, 2023),
    Month = c(9, 9),
    Day = c(1, 2),
    Rain = c(1, 1),
    Eto = c(5, 5),
    RUNOFF = c(0, 0),
    DRAIN = c(0, 0),
    TRAN = c(0, 0),
    AVAIL = c(0, 0),
    R = c(0, 0)
  )

  result <- calcWatBal(data, soilWHC = soilWHC)

  ## ***** Compare results:

  expect_equal(result$data$RUNOFF, expected_results$RUNOFF)
  expect_equal(result$data$DRAIN, expected_results$DRAIN)
  expect_equal(result$data$TRAN, expected_results$TRAN)
  expect_equal(result$data$AVAIL, expected_results$AVAIL)
  expect_equal(result$data$R, expected_results$R)
})

###############################################################################
###############################################################################
#                >>>>>>>>>>   End of code   <<<<<<<<<<                        #
###############################################################################
