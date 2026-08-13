#' Create rainfall erosivity calculation settings
#'
#' Creates a settings object controlling storm separation, storm
#' omission criteria, rainfall-intensity durations, kinetic-energy
#' calculation, and handling of single-record rainfall events.
#'
#' The principal defaults correspond to the RIST configuration used
#' during validation of this package:
#'
#' - storm-break duration: 6 hours;
#' - RIST storm-break precipitation setting: 1.27 mm;
#' - precipitation omission threshold: 12.70 mm;
#' - intensity omission threshold: 25.40 mm/h;
#' - omission intensity duration: 15 minutes;
#' - both omission criteria enabled;
#' - both enabled omission conditions must be satisfied;
#' - Brown and Foster (1987) kinetic-energy equation.
#'
#' With the default omission settings, a storm is omitted only when
#' both
#'
#' \deqn{P < 12.70\ {\rm mm}}
#'
#' and
#'
#' \deqn{I_{15} < 25.40\ {\rm mm/h}}
#'
#' are satisfied. The inequalities are strict: values equal to either
#' threshold do not satisfy that omission condition.
#'
#' The default `single_record_energy = "calculate"` differs from the
#' behaviour observed for single-record storms in RIST 3.99.10.
#' Set `single_record_energy = "rist_zero"` when that specific RIST
#' behaviour is required.
#'
#' @param storm_break_hours Positive number giving the elapsed time,
#'   in hours, used to separate rainfall events. Consecutive positive
#'   rainfall observations separated by no more than this duration
#'   belong to the same event. A greater separation starts a new event.
#'   Default is `6`.
#'
#' @param storm_break_precip_mm Non-negative precipitation value
#'   associated with the RIST storm-break configuration, in mm.
#'   Default is `1.27`.
#'
#'   Dedicated validation experiments with RIST 3.99.10 using
#'   fixed-interval rainfall input showed identical storm grouping when
#'   this value was varied from 0 to 10 mm. In the validated
#'   fixed-interval algorithm used by `Rfactor`, storm separation is
#'   therefore controlled by `storm_break_hours`; this value is retained
#'   as RIST configuration metadata and does not alter storm grouping.
#'
#' @param omit_precip_below_mm Non-negative precipitation threshold,
#'   in mm, used when the precipitation-based omission criterion is
#'   enabled. The condition is satisfied when event precipitation is
#'   strictly less than this value. Default is `12.70`.
#'
#' @param omit_intensity_below_mm_h Non-negative rainfall-intensity
#'   threshold, in mm/h, used when the intensity-based omission
#'   criterion is enabled. The condition is satisfied when the selected
#'   event maximum intensity is strictly less than this value.
#'   Default is `25.40`.
#'
#' @param omit_intensity_duration_min Duration, in minutes, of the
#'   maximum rainfall intensity used by the intensity-based omission
#'   criterion. Must be one of `5`, `10`, `15`, `30`, or `60`.
#'   Default is `15`.
#'
#'   When `omit_intensity = TRUE`, this duration must also occur in
#'   `intensity_durations_min`.
#'
#' @param omit_logic How multiple enabled omission criteria are
#'   combined. Either `"all"` or `"any"`. Default is `"all"`.
#'
#'   `"all"` omits an event only when all enabled omission conditions
#'   are satisfied. `"any"` omits an event when at least one enabled
#'   condition is satisfied.
#'
#'   When only one criterion is enabled, the choice has no practical
#'   effect. When both `omit_precip` and `omit_intensity` are `FALSE`,
#'   no omission criterion is applied and all identified events are
#'   retained.
#'
#' @param energy_equation Kinetic-energy equation. One of:
#'
#'   - `"brown_foster_1987"`;
#'   - `"mcgregor_1995"`;
#'   - `"laws_parsons_1943"`.
#'
#'   Default is `"brown_foster_1987"`.
#'
#' @param intensity_durations_min Positive rainfall-intensity durations,
#'   in minutes, to calculate for each event. Values must be finite and
#'   must not contain duplicates. They are stored in increasing order.
#'   Default is `c(5, 10, 15, 20, 30, 60)`.
#'
#'   Requested durations must be compatible with the temporal
#'   resolution of the rainfall data when intensities are calculated.
#'   If the settings are used for EI30 calculation, `30` must be
#'   included.
#'
#' @param single_record_energy How kinetic energy should be handled for
#'   an event containing only one rainfall record. Either `"calculate"`
#'   or `"rist_zero"`. Default is `"calculate"`.
#'
#'   `"calculate"` applies the selected kinetic-energy equation normally.
#'
#'   `"rist_zero"` sets event energy and EI30 to zero for a one-record
#'   event, reproducing the behaviour observed during validation against
#'   RIST 3.99.10 fixed-interval input.
#'
#' @param omit_precip Logical. Should the precipitation-based omission
#'   criterion be enabled? Default is `TRUE`.
#'
#' @param omit_intensity Logical. Should the intensity-based omission
#'   criterion be enabled? Default is `TRUE`.
#'
#' @return An object of class `rf_settings` containing the validated
#'   calculation settings.
#'
#' @examples
#' # Default settings
#' settings <- rf_settings()
#'
#' # Include every identified rainfall event
#' include_all <- rf_settings(
#'   omit_precip = FALSE,
#'   omit_intensity = FALSE
#' )
#'
#' # Example settings for 10-minute rainfall data
#' settings_10min <- rf_settings(
#'   intensity_durations_min = c(10, 20, 30, 60),
#'   omit_intensity_duration_min = 10
#' )
#'
#' @export
#'
#'
rf_settings <- function(
    storm_break_hours = 6,
    storm_break_precip_mm = 1.27,
    omit_precip_below_mm = 12.70,
    omit_intensity_below_mm_h = 25.40,
    omit_intensity_duration_min = 15,
    omit_logic = c(
      "all",
      "any"
    ),
    energy_equation = "brown_foster_1987",
    intensity_durations_min = c(
      5,
      10,
      15,
      20,
      30,
      60
    ),
    single_record_energy = c(
      "calculate",
      "rist_zero"
    ),
    omit_precip = TRUE,
    omit_intensity = TRUE
) {

  # ------------------------------------------------------------------
  # Match categorical settings
  # ------------------------------------------------------------------

  omit_logic <- match.arg(
    omit_logic
  )


  energy_equation <- match.arg(
    energy_equation,
    choices = c(
      "brown_foster_1987",
      "mcgregor_1995",
      "laws_parsons_1943"
    )
  )


  single_record_energy <- match.arg(
    single_record_energy
  )


  # ------------------------------------------------------------------
  # Validate storm-break settings
  # ------------------------------------------------------------------

  if (
    length(storm_break_hours) != 1 ||
    !is.numeric(storm_break_hours) ||
    !is.finite(storm_break_hours) ||
    storm_break_hours <= 0
  ) {
    stop(
      "storm_break_hours must be one positive finite value.",
      call. = FALSE
    )
  }


  if (
    length(storm_break_precip_mm) != 1 ||
    !is.numeric(storm_break_precip_mm) ||
    !is.finite(storm_break_precip_mm) ||
    storm_break_precip_mm < 0
  ) {
    stop(
      paste0(
        "storm_break_precip_mm must be one non-negative ",
        "finite value."
      ),
      call. = FALSE
    )
  }


  # ------------------------------------------------------------------
  # Validate omission switches
  # ------------------------------------------------------------------

  if (
    length(omit_precip) != 1 ||
    !is.logical(omit_precip) ||
    is.na(omit_precip)
  ) {
    stop(
      "omit_precip must be TRUE or FALSE.",
      call. = FALSE
    )
  }


  if (
    length(omit_intensity) != 1 ||
    !is.logical(omit_intensity) ||
    is.na(omit_intensity)
  ) {
    stop(
      "omit_intensity must be TRUE or FALSE.",
      call. = FALSE
    )
  }


  # ------------------------------------------------------------------
  # Validate omission thresholds
  # ------------------------------------------------------------------

  if (
    length(omit_precip_below_mm) != 1 ||
    !is.numeric(omit_precip_below_mm) ||
    !is.finite(omit_precip_below_mm) ||
    omit_precip_below_mm < 0
  ) {
    stop(
      paste0(
        "omit_precip_below_mm must be one non-negative ",
        "finite value."
      ),
      call. = FALSE
    )
  }


  if (
    length(omit_intensity_below_mm_h) != 1 ||
    !is.numeric(omit_intensity_below_mm_h) ||
    !is.finite(omit_intensity_below_mm_h) ||
    omit_intensity_below_mm_h < 0
  ) {
    stop(
      paste0(
        "omit_intensity_below_mm_h must be one non-negative ",
        "finite value."
      ),
      call. = FALSE
    )
  }


  allowed_omit_durations <- c(
    5,
    10,
    15,
    30,
    60
  )


  if (
    length(omit_intensity_duration_min) != 1 ||
    !is.numeric(omit_intensity_duration_min) ||
    !is.finite(omit_intensity_duration_min) ||
    !omit_intensity_duration_min %in%
    allowed_omit_durations
  ) {
    stop(
      paste0(
        "omit_intensity_duration_min must be one of: ",
        paste(
          allowed_omit_durations,
          collapse = ", "
        ),
        "."
      ),
      call. = FALSE
    )
  }


  # ------------------------------------------------------------------
  # Validate calculated intensity durations
  # ------------------------------------------------------------------

  if (
    !is.numeric(intensity_durations_min) ||
    length(intensity_durations_min) == 0 ||
    anyNA(intensity_durations_min) ||
    any(!is.finite(intensity_durations_min)) ||
    any(intensity_durations_min <= 0)
  ) {
    stop(
      paste0(
        "intensity_durations_min must contain positive ",
        "finite numeric values."
      ),
      call. = FALSE
    )
  }


  if (anyDuplicated(intensity_durations_min)) {
    stop(
      "intensity_durations_min must not contain duplicates.",
      call. = FALSE
    )
  }


  intensity_durations_min <- sort(
    intensity_durations_min
  )


  if (
    omit_intensity &&
    !omit_intensity_duration_min %in%
    intensity_durations_min
  ) {
    stop(
      paste0(
        "The selected omit_intensity_duration_min (",
        omit_intensity_duration_min,
        " minutes) must also be included in ",
        "intensity_durations_min."
      ),
      call. = FALSE
    )
  }


  # ------------------------------------------------------------------
  # Construct settings object
  # ------------------------------------------------------------------

  result <- list(

    storm_break_hours =
      storm_break_hours,

    storm_break_precip_mm =
      storm_break_precip_mm,

    omit_precip =
      omit_precip,

    omit_precip_below_mm =
      omit_precip_below_mm,

    omit_intensity =
      omit_intensity,

    omit_intensity_below_mm_h =
      omit_intensity_below_mm_h,

    omit_intensity_duration_min =
      omit_intensity_duration_min,

    omit_logic =
      omit_logic,

    energy_equation =
      energy_equation,

    intensity_durations_min =
      intensity_durations_min,

    single_record_energy =
      single_record_energy
  )


  class(result) <- "rf_settings"


  result
}


#' @export
print.rf_settings <- function(
    x,
    ...
) {

  cat(
    "<rf_settings>\n"
  )


  cat(
    "  Storm break:\n"
  )

  cat(
    "    hours:                         ",
    x$storm_break_hours,
    "\n",
    sep = ""
  )

  cat(
    "    RIST precipitation setting (mm): ",
    x$storm_break_precip_mm,
    " [metadata]",
    "\n",
    sep = ""
  )


  cat(
    "  Storm omission:\n"
  )

  cat(
    "    precipitation criterion:       ",
    if (x$omit_precip) {
      "enabled"
    } else {
      "disabled"
    },
    "\n",
    sep = ""
  )

  cat(
    "    precipitation below (mm):      ",
    x$omit_precip_below_mm,
    "\n",
    sep = ""
  )

  cat(
    "    intensity criterion:           ",
    if (x$omit_intensity) {
      "enabled"
    } else {
      "disabled"
    },
    "\n",
    sep = ""
  )

  cat(
    "    intensity below (mm/h):        ",
    x$omit_intensity_below_mm_h,
    "\n",
    sep = ""
  )

  cat(
    "    intensity interval (min):      ",
    x$omit_intensity_duration_min,
    "\n",
    sep = ""
  )

  cat(
    "    criterion logic:               ",
    x$omit_logic,
    "\n",
    sep = ""
  )


  cat(
    "  Energy equation:                 ",
    x$energy_equation,
    "\n",
    sep = ""
  )


  cat(
    "  Calculated intensities (min):    ",
    paste(
      x$intensity_durations_min,
      collapse = ", "
    ),
    "\n",
    sep = ""
  )


  cat(
    "  Single-record energy:            ",
    x$single_record_energy,
    "\n",
    sep = ""
  )


  invisible(x)
}
