test_seasFcstQBR <- function() {
  test_that("seasFcstQBR function works correctly", {
    ## ***** Test valid inputs:
    hisYearStart <- 2000
    hisYearEnd <- 2010
    rainTerc <- c(0.33, 0.33, 0.34)
    seasRain <- data.frame(Year = 2000:2010, sRain = runif(11, 0, 1000))
    hisWSCvar <- data.frame(
      Year = 2000:2010, OnsetValue = sample(1:366, 11),
      CessationValue = sample(1:366, 11)
    )
    fcstVarName <- "Onset"
    tercileMethod <- "quantiles"

    result <- seasFcstQBR(
      hisYearStart, hisYearEnd, rainTerc,
      seasRain, hisWSCvar, fcstVarName, tercileMethod
    )
    expect_is(result, "data.frame")
    expect_equal(ncol(result), 3)
    expect_named(result, c("BelowNormal", "Normal", "AboveNormal"))

    ## ***** Test invalid tercile probabilities:
    expect_error(
      seasFcstQBR(
        hisYearStart, hisYearEnd, c(0.3, 0.3, 0.3),
        seasRain, hisWSCvar, fcstVarName, tercileMethod
      ),
      "Tercile probabilities must sum to 1."
    )

    ## ***** Test invalid fcstVarName:
    expect_error(
      seasFcstQBR(
        hisYearStart, hisYearEnd, rainTerc, seasRain,
        hisWSCvar, "InvalidName", tercileMethod
      ),
      "fcstVarName must be either 'Onset' or 'Cessation'."
    )

    ## ***** Test invalid tercileMethod:
    expect_error(
      seasFcstQBR(
        hisYearStart, hisYearEnd, rainTerc,
        seasRain, hisWSCvar, fcstVarName, "invalidMethod"
      ),
      "tercileMethod must be either 'quantiles' or 'fixedValues'."
    )

    ## ***** Test invalid year range:
    expect_error(
      seasFcstQBR(
        2010, 2000, rainTerc, seasRain, hisWSCvar,
        fcstVarName, tercileMethod
      ),
      "hisYearStart and hisYearEnd must be numeric, and hisYearStart
                 should be less than or equal to hisYearEnd."
    )

    ## ***** Test missing columns in seasRain:
    invalidSeasRain <- data.frame(Year = 2000:2010)
    expect_error(
      seasFcstQBR(
        hisYearStart, hisYearEnd, rainTerc,
        invalidSeasRain, hisWSCvar, fcstVarName,
        tercileMethod
      ),
      "seasRain must contain columns 'Year' and 'sRain'."
    )

    ## ***** Test missing columns in hisWSCvar:
    invalidHisWSCvar <- data.frame(Year = 2000:2010)
    expect_error(
      seasFcstQBR(
        hisYearStart, hisYearEnd, rainTerc, seasRain,
        invalidHisWSCvar, fcstVarName, tercileMethod
      ),
      "hisWSCvar must contain a 'Year' column."
    )

    ## ***** Test unrealistic values in hisWSCvar:
    unrealisticHisWSCvar <- data.frame(
      Year = 2000:2010,
      OnsetValue = c(
        1, 2, 3, 4, 5, 6, 7,
        8, 9, 10, 400
      )
    )
    expect_warning(
      seasFcstQBR(
        hisYearStart, hisYearEnd, rainTerc, seasRain,
        unrealisticHisWSCvar, fcstVarName, tercileMethod
      ),
      "hisWSCvar contains unrealistic values"
    )

    ## ***** Test negative values in seasRain:
    negativeSeasRain <- data.frame(
      Year = 2000:2010,
      sRain = c(-1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
    )
    expect_warning(
      seasFcstQBR(
        hisYearStart, hisYearEnd, rainTerc,
        negativeSeasRain, hisWSCvar, fcstVarName,
        tercileMethod
      ),
      "seasRain contains negative rainfall values"
    )
  })
}


###############################################################################
###############################################################################
#                >>>>>>>>>>   End of code   <<<<<<<<<<                        #
###############################################################################
