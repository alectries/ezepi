# ezepi Changelog

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
