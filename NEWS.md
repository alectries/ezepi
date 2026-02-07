# ezepi Changelog

## ezepi 2.5.1

### Changed

- Added Mantel-Haenszel functionality to calc
- Removed fmsb function calls from mh functions, changing their dependency to calc

## ezepi 2.5.0

### Added

- **calc**: Added calc function to calculate risk difference, risk ratio, odds ratio, incidence rate difference, and incidence rate ratio natively with CI and p-value.

### Changed

- Removed fmsb function calls from ez and more functions, changing their dependency to new calc function.

## ezepi 2.4.2

### Changed

- Added options for bar color, fill, and width to ezcurve call.

## ezepi 2.4.1

### Changed

- Fixed bar width of epi curves.

## ezepi 2.4.0

### Added

- **print option.** Added option to silence counts table in console to association measure commands.

### Changed

- Changed numeric date warning message.

## ezepi 2.3.0

### Added

- **mh functions.** Mantel-Haenszel implementation.

### Changed

- Moved table printing to table helper function and added print argument.

## ezepi 2.2.0

### Added

- **ezcurve.** Generate a simple but highly customizable epi curve using ggplot2.

## ezepi 2.1.0

### Added

- **Error checking.**: Added startup error checks to ezepi.

### Changed

- **Piping fixes.**: Fixed non-standard evaluation to support dataset piping again.
- **Row mutation fixes.**: Fixed mutate_rows, which had broken after changes to ezt.

## ezepi 2.0.1
- **Bug fixes**: Fixed some scattered import issues. Fixed table outputs being grouped.

## ezepi 2.0.0

- **Refactored all code.** ezepi is now modular, easier to read (and maintain), and computationally cheaper. This was accomplished by:
  - Drastically reducing the number of external functions used.
  - Eliminating repeat calculations by saving intermediate states more often.
  - Importing individual external functions rather than their packages. (I'm learning!)
- **Changed results format.** Results are still printed in a human-readable tibble, but values are now printed in columns, so you can do something like ezior(...)$`Odds Ratio` to easily retrieve just the estimate.
- **Added moreird and moreirr.** You can now calculate rate measures across multiple exposures.

## ezepi 1.0.0

This is the first release version of ezepi. Tests are now passing, and the package is considered stable. New releases will be feature releases.

### Added

- Finished adding tests to ezepi.

### Changed

- Updated depends to replace tidyverse, which is a meta-package, with the individual packages actually used by ezepi.
- Changed moreprev to output a nicer and more useful table.

## ezepi 0.0.4

### Changed

- Fixed `ezt()` and `mutate_rows()` to allow non-numeric data.

## ezepi 0.0.3

### Added

#### Functions
- `moreprev`, which is like `ezprev` but allows for categorical, not just binary, variables.
- `ezt`, which transposes a dataframe and (unlike `t`) can preserve headers.
- `mutate_rows`, like `mutate` from `dplyr`, generates row values based on other rows.

#### Unit testing
- IN PROGRESS: Tests are being added to verify that the code is working properly before the package is installed on your device. This will not impact usage.

## ezepi 0.0.2

### Added

#### Changelog

- This changelog!

#### Sanity checking

- All functions check that exposure, outcome, and person-time (if applicable) variables are given
- Functions print input information to the log for manual review
- Functions check that index/ref inputs match exposure/outcome variable type
- Functions check that index/ref inputs exist in exposure/outcome variable

### Changed

- Solved some unnecessary warning messages
- ez function result outputs are now numeric rather than character

## ezepi 0.0.1

Initial release.
