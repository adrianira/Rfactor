# Rfactor 0.2.0

## New features

* Added `rf_calculate_mean_rfactor()` for calculating multi-year mean
  rainfall erosivity from monthly or yearly R-factor values.

* For yearly input, the function calculates the arithmetic mean of available
  yearly R-factor values.

* For monthly input, the function calculates a separate multi-year mean for
  each available calendar month.

* Missing `R` values are excluded from multi-year means, while genuine
  `R = 0` values are retained.

* Added `n_years` to report the number of non-missing R-factor values
  contributing to each multi-year mean.

* Missing years and month-year combinations are not generated or imputed.
  Calendar months completely absent from the supplied input are not created.

* Mean annual rainfall erosivity is calculated directly from yearly R-factor
  values rather than by summing multi-year monthly means.

## Documentation

* Extended the package workflow documentation to distinguish monthly and
  yearly erosivity totals from multi-year mean rainfall erosivity.

* Updated the README and getting-started vignette with examples of
  `rf_calculate_mean_rfactor()`.

* Updated the methods and validation vignette to document multi-year
  averaging rules and their relationship to record completeness and
  representativeness.

* Added links to the official USDA Agricultural Research Service RIST
  resources in the validation documentation.

## Testing

* Added tests for yearly and monthly multi-year averaging, including handling
  of missing values, genuine zero values, absent calendar months, duplicate
  periods, and invalid year, month, and R-factor values.


# Rfactor 0.1.0

## Initial release

* Added tools for reading and validating timestamped precipitation records,
  including sparse records with temporal gaps.

* Added configurable rainfall-event identification using an elapsed
  storm-break duration.

* Added continuous rolling maximum rainfall-intensity calculations.

* Added rainfall kinetic-energy calculations using:
  - Brown and Foster (1987);
  - McGregor et al. (1995);
  - Laws and Parsons (1943).

* Added event EI30 rainfall-erosivity calculations and configurable
  precipitation- and intensity-based event omission criteria.

* Added monthly and yearly aggregation of contributing event EI30 values.

* Added support for configurable rainfall-intensity durations and
  fixed-interval rainfall resolutions compatible with EI30 calculation.

* Added optional RIST-compatible handling of single-record rainfall events.

* Added empirical validation against RIST 3.99.10 using synthetic
  fixed-interval rainfall data, including:
  - storm-break boundary and intermediate-rainfall experiments;
  - strict omission-threshold tests;
  - kinetic-energy equation comparisons;
  - 1-, 5-, 10-, 15-, and 30-minute temporal-resolution comparisons.

* Added validation with real high-resolution and sparse historical
  precipitation records from Meteo Romania (National Meteorological
  Administration). Original Meteo Romania precipitation records are not
  distributed with the package or public repository.

* Added package documentation, a getting-started vignette, and a methods
  and validation vignette.
