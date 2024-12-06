# ezepi Changelog

## ezepi 2.0.1
- **Bug fixes**: Fixed some scattered import issues.

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
