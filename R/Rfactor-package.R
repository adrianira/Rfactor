#' Rfactor: Rainfall Erosivity Calculations from Precipitation Records
#'
#' @description
#' `Rfactor` provides tools for calculating rainfall erosivity from
#' precipitation records. The package identifies rainfall events,
#' calculates maximum rainfall intensities, rainfall kinetic energy,
#' and event EI30 erosivity, aggregates contributing event erosivity to
#' monthly and yearly totals, and calculates multi-year mean rainfall
#' erosivity from those period totals.
#'
#' Monthly and yearly values returned by [rf_calculate_rfactor()]
#' represent erosivity calculated from the rainfall data available for
#' those calendar periods. They are period totals and should not
#' automatically be interpreted as climatological R-factor values.
#'
#' Multi-year mean monthly or annual rainfall erosivity can be calculated
#' from these period totals with [rf_calculate_mean_rfactor()].
#' Interpretation as a representative climatic R-factor still requires
#' an appropriate rainfall record and consideration of its completeness
#' and representativeness.
#'
#' @section Main workflow:
#'
#' A typical analysis consists of:
#'
#' 1. reading rainfall data with [rf_read_rainfall()];
#' 2. validating the rainfall record with [rf_validate_rainfall()];
#' 3. identifying rainfall events with [rf_identify_storms()];
#' 4. calculating event energy and EI30 with [rf_calculate_ei30()];
#' 5. aggregating contributing event erosivity to monthly or yearly
#'    totals with [rf_calculate_rfactor()];
#' 6. calculating multi-year mean monthly or annual rainfall erosivity
#'    with [rf_calculate_mean_rfactor()].
#'
#' Calculation settings are created with [rf_settings()].
#'
#' @section Calculation settings:
#'
#' `Rfactor` allows the user to configure storm-break duration,
#' event-omission criteria, omission logic, rainfall-intensity
#' durations, and the kinetic-energy equation.
#'
#' Three kinetic-energy equations are available:
#'
#' - Brown and Foster (1987);
#' - McGregor et al. (1995);
#' - Laws and Parsons (1943).
#'
#' Most default settings correspond to the RIST configuration used
#' during package validation. The exception is single-record event
#' energy: `single_record_energy = "calculate"` is the Rfactor default.
#' Use `single_record_energy = "rist_zero"` to reproduce the zero-energy
#' behaviour observed for single-record events in RIST 3.99.10
#' fixed-interval input.
#'
#' @section Period totals and multi-year means:
#'
#' [rf_calculate_rfactor()] sums the EI30 values of contributing erosive
#' events within represented calendar months or years.
#'
#' [rf_calculate_mean_rfactor()] calculates arithmetic multi-year means
#' from those monthly or yearly R-factor values.
#'
#' For yearly input, the function calculates the mean of the available
#' yearly R-factor values. For monthly input, a separate multi-year mean
#' is calculated for each available calendar month.
#'
#' Missing `R` values are excluded from the mean. Genuine zero values are
#' retained because `R = 0` represents a valid period with no contributing
#' erosive events.
#'
#' Missing years or month-year combinations are not generated or imputed.
#' The number of non-missing years contributing to each mean is reported
#' as `n_years`.
#'
#' Mean annual rainfall erosivity is calculated directly from yearly
#' R-factor values. It is not obtained by summing the twelve multi-year
#' monthly means, because data availability may differ among calendar
#' months.
#'
#' @section Temporal resolution:
#'
#' EI30 requires rainfall data with sufficient temporal resolution to
#' calculate a genuine 30-minute maximum intensity. Fixed-interval
#' rainfall calculations have been validated against RIST 3.99.10 using
#' 1-, 5-, 10-, 15-, and 30-minute data.
#'
#' Requested rainfall-intensity durations must be compatible with the
#' observation interval. For example, 10-minute rainfall data can be
#' used to calculate 10-, 20-, 30-, and 60-minute intensities, but not
#' a true 5- or 15-minute intensity.
#'
#' Rainfall records coarser than 30 minutes do not contain enough
#' information to calculate a genuine EI30 value.
#'
#' @section Incomplete and sparse records:
#'
#' `Rfactor` calculates erosivity from the rainfall observations
#' supplied by the user. It does not assess, estimate, or correct the
#' climatological completeness of a rainfall record.
#'
#' Gaps between available observations therefore do not prevent
#' calculation. Missing months or years are not created or imputed.
#' Within an identified rainfall event, the expected regular time grid
#' is reconstructed for calculation from the supplied rainfall record.
#'
#' The same principle applies to multi-year means. An available monthly
#' or yearly R-factor value is treated as usable unless it is missing.
#' The package does not determine whether that value was derived from a
#' complete rainfall record. Assessment of completeness and
#' representativeness remains the responsibility of the user.
#'
#' @section Scope:
#'
#' The package focuses on rainfall erosivity calculations. It does not
#' calculate the other USLE or RUSLE factors and does not perform
#' spatial interpolation or mapping of the R-factor.
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
