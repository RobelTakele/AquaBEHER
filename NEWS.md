# AquaBEHER Changelog

---

## Version 1.2.0

### 🆕 Added

- **📖 Citation Information**: Citation details are now included in `inst/CITATION`, enabling proper referencing.
  
- **📊 CRAN Downloads Badge**: A badge displaying CRAN downloads has been added to the `README` to track package usage.

- **✅ Data Quality Control**: New data validation checks have been introduced to ensure the integrity of inputs for water balance calculations. These include realistic value checks and error handling.

- **🧪 Testing Structure**: Established comprehensive unit tests for the `calcWatBal` and `calcSeasCal` functions using `testthat`, ensuring better test coverage and reliability.

- **⚙️ Parameter Validation**: Enhanced validation mechanisms in the `calcSeasCal` function to ensure accurate input. Improvements include:
  
  - **🔍 Consistency Checks**: Ensures that date-related columns (Year, Month, Day) are complete and contain no missing values.
  
  - **📅 Date Validation**: Ensures `onsetWind.start`, `onsetWind.end`, and `cessaWind.end` are valid dates using `lubridate::is.Date()`.
  
  - **📐 Range Validation**: Confirms that `R-index` values fall between 0 and 1, while `Soil Moisture` values remain non-negative.
  
  - **📏 Length Consistency**: Ensures `R-index` and `Soil Moisture` arrays match the length of the Year column.
  
  - **🌱 soilWHC Validation**: Verifies that `soilWHC` is a positive numeric value for accurate soil water holding capacity calculations.

### 🔄 Changed

- **📦 Package Imports**: Transitioned from `raster` to `terra` for more efficient spatial data processing.
  
- **📈 calcWatBal Function**: Now returns a list that includes:
  - A dataframe with results.
  - A list of warnings for unrealistic or adjusted values in the input data, making error tracing easier.

- **📅 Wet Season Calendar (WSC) Parameters**: Improved logic and criteria for characterizing the onset and cessation of the wet season, ensuring more accurate agroclimatic analysis.

---

## Version 1.0.0

### 🆕 Added

- **🛠️ Error Handling**: Enhanced error management for missing or unrealistic values, making the package more robust and user-friendly.

- **⚖️ Consistency Checks**: Implemented validation to ensure that maximum temperature is always greater than minimum temperature, avoiding input errors.
  
- **📝 Documentation**: Significantly improved package documentation for better clarity and completeness, including examples and usage guidance.

- **🧪 Testing**: Introduced unit testing for the `calcEto` function, further improving the reliability of core calculations.

---

## Version 0.1.0

### 🆕 Initial Release

- **🌱 AquaBEHER 0.1.0**: Initial submission to CRAN, introducing essential functionalities for estimating soil water balance and wet season characterization.

---

