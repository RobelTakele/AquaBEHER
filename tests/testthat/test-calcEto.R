
testthat::test_that("calcEto warns for unrealistic temperature values", {

  ## Create mock dataset with unrealistic temperatures

  data <- data.frame(
    Tmax = c(65, -55, 25),
    Tmin = c(55, -65, 20),
    Lat = 41.89,
    Lon = 12.48,
    Elev = 100,
    Year = 2023,
    Month = c(7, 8, 9),
    Day = c(1, 15, 30)
  )

  ## Expect a warning about unrealistic temperatures

  expect_warning(calcEto(data, method = "HS"))

})

testthat::test_that("calcEto stops for missing required data (Tmax, Tmin)", {

  ## Create data with missing Tmax and Tmin

  data <- data.frame(
    Lat = 41.89,
    Lon = 12.48,
    Elev = 100,
    Year = 2023,
    Month = c(7, 8, 9),
    Day = c(1, 15, 30)
  )

  ## Expect an error about missing data

  expect_error(calcEto(data))

})

testthat::test_that("calcEto adjusts inconsistent temperature data", {

  ## Create mock data with Tmin greater than Tmax

  data <- data.frame(
    Tmax = c(25, 20, 25),
    Tmin = c(30, 22, 20),
    Lat = 41.89,
    Lon = 12.48,
    Elev = 100,
    Year = 2023,
    Month = c(7, 8, 9),
    Day = c(1, 15, 30)
  )

  ## Call the function and capture the message

  output <- testthat::capture_messages(calcEto(data, method = "HS"))

  ## Print the captured output for debugging

  print(output)

  ## Expect a message about adjusting inconsistent temperatures

  testthat::expect_true(any(stringr::str_detect(output, "Adjusted")))

})

testthat::test_that("calcEto stops for invalid method", {

  ## Create data

  data <- data.frame(
    Tmax = c(25, 20, 25),
    Tmin = c(20, 18, 22),
    Lat = 41.89,
    Lon = 12.48,
    Elev = 100,
    Year = 2023,
    Month = c(7, 8, 9),
    Day = c(1, 15, 30)
  )

  ## Test with invalid method

  expect_error(calcEto(data, method = "invalid"))

})

test_that("calcEto handles valid input correctly", {

  ## ***** Create a mock dataset

  data <- data.frame(
    Year = rep(2024, 2),
    Month = rep(9, 2),
    Day = 1:2,
    Lat = c(50, 50),
    Elev = rep(100, 2),
    Tmax = c(25, 27),
    Tmin = c(15, 17),
    Rs = c(15, 14),
    U2 = c(2, 3),
    RHmax = c(85, 80),
    RHmin = c(40, 45)
  )

  testthat::test_that("calcEto calculates Hargreaves-Samani ET (HS)", {

    # Create mock data
    data <- data.frame(
      Tmax = c(25, 20, 25),
      Tmin = c(20, 18, 22),
      Lat = 41.89,
      Lon = 12.48,
      Elev = 100,
      Year = 2023,
      Month = c(7, 8, 9),
      Day = c(1, 15, 30)
    )

    ## Calculate ET using the Hargreaves-Samani method

    result <- calcEto(data, method = "HS")

    ## Expected ET values (based on Hargreaves-Samani formula)

    expected_et <- c(4.802557, 3.361835, 2.878480)

    ## Check if the calculated ET values are approximately equal to
    ## the expected values

    testthat::expect_equal(result$ET.Daily, expected_et, tolerance = 0.001)

  })

  testthat::test_that("Universal constants and calculations are correct", {

    ## Create mock data

    data <- data.frame(
      Tmax = c(25, 28, 26),
      Tmin = c(15, 17, 16),
      Rs = c(15, 16, 14),
      Lat = 41.89,
      Lon = 12.48,
      Elev = 100,
      Year = 2023,
      Month = c(7, 8, 9),
      Day = c(1, 15, 30),
      RHmax = c(80, 75, 85),
      RHmin = c(50, 45, 55)
    )

    ## ***** Universal constants *****

    lambda <- 2.45
    Cp <- 1.013e-3
    e <- 0.622
    Sigma <- 4.903e-09
    Gsc <- 0.082
    G <- 0
    alphaPT <- 1.26
    alpha <- 0.23

    # ***** Latitude in Radians *****
    lat.rad <- data$Lat * (pi / 180)

    # ***** Date and Elevation Data *****
    date.vec <- as.Date(paste0(data$Year, "-", data$Month, "-", data$Day))
    data$J <- as.numeric(format(date.vec, "%j"))
    Elev <- unique(data$Elev)

    # Ensure only one unique Elevation
    testthat::expect_length(Elev, 1)

    # Check required variables are present
    reqVars <- c("Tmax", "Tmin", "Rs")
    misVars <- reqVars[!reqVars %in% names(data)]
    testthat::expect_equal(length(misVars), 0)

    # ***** Mean temperature calculation *****
    Tavg <- (data$Tmax + data$Tmin) / 2

    # ***** Vapor Pressure Calculations *****
    EsTmax <- 0.6108 * exp(17.27 * data$Tmax / (data$Tmax + 237.3))
    EsTmin <- 0.6108 * exp(17.27 * data$Tmin / (data$Tmin + 237.3))
    Es <- (EsTmax + EsTmin) / 2
    Ea <- (EsTmin * data$RHmax / 100 + EsTmax * data$RHmin / 100) / 2

    # ***** Atmospheric Pressure *****
    P <- 101.3 * ((293 - 0.0065 * Elev) / 293)^5.26

    # ***** Slope of Saturation Vapor Pressure Curve *****
    delta <- 4098 * (0.6108 * exp((17.27 * Tavg) / (Tavg + 237.3))) /
      ((Tavg + 237.3)^2)

    # ***** Psychrometric constant *****
    gamma <- (Cp * P) / (lambda * e)

    # ***** Inverse relative distance Earth-Sun *****
    dr <- 1 + 0.033 * cos(2 * pi / 365 * data$J)

    # ***** Solar Declination *****
    SDc <- 0.409 * sin(2 * pi / 365 * as.numeric(data$J) - 1.39)

    # ***** Sunset Hour Angle *****
    Ws <- acos(-tan(lat.rad) * tan(SDc))

    # ***** Daylight hours *****
    N <- 24 / pi * Ws

    # ***** Extraterrestrial radiation *****
    Ra <- (1440 / pi) * dr * Gsc * (Ws * sin(lat.rad) * sin(SDc) +
                                      cos(lat.rad) * cos(SDc) * sin(Ws))

    # ***** Clear-sky solar radiation *****
    Rso <- (0.75 + (2 * 10^-5) * Elev) * Ra

    Rs <- data$Rs

    # ***** Net Outgoing Longwave Radiation *****
    Rnl <- Sigma * (0.34 - 0.14 * sqrt(Ea)) *
      ((data$Tmax + 273.2)^4 + (data$Tmin + 273.2)^4) / 2 *
      (1.35 * Rs / Rso - 0.35)

    # ***** Net Incoming Shortwave Radiation *****
    Rnsg <- (1 - alpha) * Rs

    # ***** Net Radiation *****
    Rng <- Rnsg - Rnl

    # ***** Potential Evapotranspiration *****
  E.PT.Daily <- alphaPT * (delta / (delta + gamma) * Rng / lambda - G / lambda)

    # Run the actual function and capture the result
    result <- calcEto(data, method = "PT")

    # Compare the calculated values
    testthat::expect_equal(result$ET.Daily, E.PT.Daily, tolerance = 0.001)
    testthat::expect_equal(result$Ra.Daily, Ra, tolerance = 0.001)
    testthat::expect_equal(result$Slope.Daily, delta, tolerance = 0.001)
    testthat::expect_equal(result$Ea.Daily, Ea, tolerance = 0.001)
    testthat::expect_equal(result$Es.Daily, Es, tolerance = 0.001)

    # Check for message output (if applicable)
    testthat::expect_message(calcEto(data, method = "PT"), "Priestley-Taylor")
  })

  ## ***** Call the function with "short" crop

   result <- calcEto(data, crop = "short")

  ## ***** Check that the result is a list

   expect_s3_class(result, "PEToutList")

  ## ***** Check that all required components are present in the result

  expect_true(all(c(
    "ET.Daily", "Ra.Daily", "Slope.Daily", "Ea.Daily",
    "Es.Daily", "ET.formulation", "ET.type"
  ) %in%
    names(result)))

  ## Check for non-NA values in the output
  expect_false(any(is.na(result$ET.Daily)))
  expect_false(any(is.na(result$Ra.Daily)))

})

test_that("calcEto handles missing data correctly", {

  ## Create a mock dataset with missing Rs

  data_missing <- data.frame(
    Year = rep(2024, 2),
    Month = rep(9, 2),
    Day = 1:2,
    Lat = c(50, 50),
    Elev = rep(100, 2),
    Tmax = c(25, 27),
    Tmin = c(15, 17),
    # Rs is missing
    U2 = c(2, 3),
    RHmax = c(85, 80),
    RHmin = c(40, 45)
  )

  ## Check that the function throws an error

  expect_error(
    calcEto(data_missing, crop = "short"),
    "Required data missing for 'Rs'"
  )
})

test_that("calcEto handles different crop types correctly", {

  ## Create a mock dataset

  data <- data.frame(
    Year = rep(2024, 2),
    Month = rep(9, 2),
    Day = 1:2,
    Lat = c(50, 50),
    Elev = rep(100, 2),
    Tmax = c(25, 27),
    Tmin = c(15, 17),
    Rs = c(15, 14),
    U2 = c(2, 3),
    RHmax = c(85, 80),
    RHmin = c(40, 45)
  )

  ## Test for "tall" crop

  result_tall <- calcEto(data, crop = "tall")
  expect_s3_class(result_tall, "PEToutList")
  expect_equal(
    result_tall$ET.formulation,
    "Penman-Monteith ASCE-EWRI Standardised"
  )

  ## Test for invalid crop type

  expect_error(
    calcEto(data, crop = "invalid"),
    "Invalid crop type. Choose either 'short' or 'tall'."
  )
})

testthat::test_that("Universal constants and conversions are correct", {
  # Create mock data
  data <- data.frame(
    Tmax = c(25, 28, 26),
    Tmin = c(15, 17, 16),
    Rs = c(15, 16, 14), # Ensure Rs is present
    Lat = 41.89,
    Lon = 12.48,
    Elev = 100,
    Year = 2023,
    Month = c(7, 8, 9),
    Day = c(1, 15, 30),
    RHmax = c(80, 75, 85),
    RHmin = c(50, 45, 55),
    U2 = c(2, 3, 2.5) # Wind speed data
  )

  # Test valid calculation for "short" crop
  result_short <- calcEto(data, crop = "short", method = "PM", Zh = 10)

  # Validate results for the short crop
  testthat::expect_true(inherits(result_short, "PEToutList"))
  testthat::expect_true(all(c("ET.Daily", "Ra.Daily", "Slope.Daily",
                              "Ea.Daily", "Es.Daily", "ET.formulation",
                              "ET.type") %in% names(result_short)))

  # Test valid calculation for "tall" crop
  result_tall <- calcEto(data, crop = "tall", method = "PM", Zh = 10)

  # Validate results for the tall crop
  testthat::expect_true(inherits(result_tall, "PEToutList"))
  testthat::expect_true(all(c("ET.Daily", "Ra.Daily", "Slope.Daily",
                              "Ea.Daily", "Es.Daily", "ET.formulation",
                              "ET.type") %in% names(result_tall)))

  # Ensure function handles missing solar radiation data
  data_no_Rs <- data
  data_no_Rs$Rs <- NULL

  testthat::expect_error({
    calcEto(data_no_Rs, crop = "short", method = "PM", Zh = 10)
  }, "Required data missing for 'Rs'")

  # Ensure function handles missing wind speed data
  data_no_U2 <- data
  data_no_U2$U2 <- NULL

  testthat::expect_error({
    calcEto(data_no_U2, crop = "short", method = "PM", Zh = 10)
  }, "Required data missing for 'Uz' or 'U2'")


  # Test message output
  testthat::expect_message(calcEto(data, crop = "short", method = "PM",
                                   Zh = 10), "Penman-Monteith FAO56")
})
