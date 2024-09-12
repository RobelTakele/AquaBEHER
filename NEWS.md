# Changelog

# AquaBEHER 1.2.0 
## 2024-09-09

### Added
- **Citation Information**: Included citation details in `inst/CITATION`.
- **CRAN Downloads Badge**: Added a badge for CRAN downloads in the README.
- **Data Quality Control**: Introduced checks for data quality control and realistic values in water balance calculations.
- **Testing Structure**: Established a testing structure for the `calcWatBal` and `calcSeasCal` functions using `testthat`.
- **Parameter Validation**: Enhanced and more robust parameter validation checks for the `calcSeasCal` function, including:
- **Consistency Checks**: Ensures all date-related columns (Year, Month, Day) are complete and free of NA values.
- **Date Validation**: Validates `onsetWind.start`, `onsetWind.end`, and `cessaWind.end` as proper dates using `lubridate::is.Date()`.
- **Range Validation**: Ensures `R-index` values are between 0 and 1, and `Soil Moisture` values are non-negative.
- **Length Consistency**: Confirms `R-index` and `Soil Moisture` have the same length as the Year column.
- **soilWHC Validation**: Checks that `soilWHC` is a positive numeric value.

### Changed
- **Package Imports**: Now imports functions from `terra` instead of `raster`.
- **calcWatBal Function**: Returns a list containing a dataframe and a list of warnings related to any unrealistic or adjusted values in the input data or parameters.
- **Wet Season Calendar (WSC) Parameters**: Improved criteria for characterizing WSC parameters.

# AquaBEHER 1.0.0 
## 2024-09-04

### Added
- **Error Handling**: Enhanced error handling and warnings for missing and unrealistic data.
- **Consistency Check**: Implemented checks to ensure the maximum temperature is always greater than the minimum temperature.
- **Documentation**: Improved documentation for better clarity and completeness.
- **Testing**: Established a testing structure for the `calcEto` function using `testthat`.

# AquaBEHER 0.1.0 
## Initial Release:
- **AquaBEHER 0.1.0**: First submission to CRAN.
