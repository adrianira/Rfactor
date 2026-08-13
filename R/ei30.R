#' Calculate event rainfall erosivity and EI30
#'
#' Calculates rainfall amount, maximum rolling rainfall intensities,
#' kinetic energy, EI30 erosivity, and erosive-event classification for
#' rainfall events produced by [rf_identify_storms()].
#'
#' @section EI30 erosivity:
#'
#' Event rainfall erosivity is calculated as
#'
#' \deqn{
#' EI_{30} = E \times I_{30}
#' }
#'
#' where `E` is total event rainfall kinetic energy in MJ/ha and
#' `I30` is the maximum continuous 30-minute rainfall intensity in
#' mm/h. EI30 therefore has units of MJ mm/(ha h).
#'
#' Maximum rainfall intensities are calculated with
#' [rf_max_intensity()] using continuous rolling windows rather than
#' fixed clock-period blocks.
#'
#' @section Temporal resolution:
#'
#' Calculation of EI30 requires rainfall data capable of representing
#' an exact 30-minute intensity window. `interval_min` must therefore
#' divide 30 minutes exactly.
#'
#' Every duration requested in `settings$intensity_durations_min` must
#' also be an exact multiple of `interval_min`.
#'
#' Fixed-interval calculations have been validated against RIST 3.99.10
#' using 1-, 5-, 10-, 15-, and 30-minute rainfall data. Rainfall data
#' with intervals greater than 30 minutes cannot provide a genuine I30
#' and therefore cannot be used for EI30 calculation.
#'
#' Temporal aggregation can change maximum intensity, kinetic energy,
#' and EI30 even when total rainfall is unchanged. Results calculated
#' from different source resolutions should therefore not be expected
#' to be identical.
#'
#' @section Event omission:
#'
#' Event omission is controlled by [rf_settings()].
#'
#' The precipitation criterion is enabled with `omit_precip` and is
#' satisfied when total event precipitation is strictly less than
#' `omit_precip_below_mm`.
#'
#' The intensity criterion is enabled with `omit_intensity` and is
#' satisfied when the selected maximum rolling intensity is strictly
#' less than `omit_intensity_below_mm_h`. The intensity duration is
#' selected with `omit_intensity_duration_min`.
#'
#' When both criteria are enabled:
#'
#' - `omit_logic = "all"` omits an event only when both conditions
#'   are satisfied;
#' - `omit_logic = "any"` omits an event when either condition is
#'   satisfied.
#'
#' Exact threshold values do not satisfy a criterion because the
#' comparisons are strict (`<`).
#'
#' If only one omission criterion is enabled, only that criterion is
#' evaluated. If both `omit_precip` and `omit_intensity` are `FALSE`,
#' no events are omitted. This corresponds to including all identified
#' rainfall events.
#'
#' For disabled criteria, the corresponding
#' `omit_precip_condition` or `omit_intensity_condition` column is
#' returned as `NA`, indicating that the condition was not evaluated.
#'
#' @section Single-record events:
#'
#' RIST 3.99.10 was observed during fixed-interval validation to assign
#' zero kinetic energy and zero EI30 to events represented by only one
#' rainfall record.
#'
#' Set `single_record_energy = "rist_zero"` in [rf_settings()] to
#' reproduce this behaviour. With the default
#' `single_record_energy = "calculate"`, the selected kinetic-energy
#' equation is applied normally.
#'
#' @param storms A rainfall-event data frame produced by
#'   [rf_identify_storms()].
#'
#' @param settings An `rf_settings` object created by [rf_settings()].
#'   If `NULL`, the settings stored in `storms` are used. If no stored
#'   settings are available, default settings from [rf_settings()] are
#'   used.
#'
#' @param interval_min Positive finite number giving the temporal
#'   resolution of the rainfall-event grid, in minutes. If `NULL`, the
#'   `interval_min` attribute stored by [rf_identify_storms()] is used.
#'   The interval must divide 30 minutes exactly, and all requested
#'   intensity durations must be exact multiples of the interval.
#'
#' @return A data frame of class `rf_events` with one row per rainfall
#'   event. It contains:
#'
#' - `storm_id`: event identifier;
#' - `event_start`: timestamp of the first event interval;
#' - `event_end`: timestamp of the final event interval;
#' - `duration_min`: represented event duration, in minutes;
#' - `precip_mm`: total event precipitation, in mm;
#' - `energy_mj_ha`: total rainfall kinetic energy, in MJ/ha;
#' - `ei30`: event EI30 erosivity, in MJ mm/(ha h);
#' - `omit_precip_condition`: result of the precipitation omission
#'   condition, or `NA` when disabled;
#' - `omit_intensity_condition`: result of the intensity omission
#'   condition, or `NA` when disabled;
#' - `omitted`: whether the event satisfies the configured omission
#'   rule;
#' - `erosive`: logical inverse of `omitted`;
#' - intensity columns named `i<duration>_mm_h` for each duration
#'   requested in `settings$intensity_durations_min`.
#'
#' The returned object also stores `settings` and `interval_min`
#' attributes.
#'
#' If `storms` contains no events, an empty `rf_events` object with the
#' corresponding output structure is returned.
#'
#' @examples
#' rainfall <- data.frame(
#'   datetime = as.POSIXct(
#'     "2025-01-01 00:00:00",
#'     tz = "UTC"
#'   ) +
#'     seq(
#'       from = 0,
#'       by = 60,
#'       length.out = 30
#'     ),
#'   precip_mm = rep(1, 30)
#' )
#'
#' storms <- rf_identify_storms(
#'   rainfall,
#'   interval_min = 1
#' )
#'
#' events <- rf_calculate_ei30(
#'   storms
#' )
#'
#' events
#'
#' @export


rf_calculate_ei30 <- function(
    storms,
    settings = NULL,
    interval_min = NULL
) {

  # ------------------------------------------------------------------
  # Required columns
  # ------------------------------------------------------------------

  required <- c(
    "storm_id",
    "datetime",
    "precip_mm"
  )


  missing_columns <- setdiff(
    required,
    names(storms)
  )


  if (length(missing_columns) > 0) {
    stop(
      paste0(
        "Missing required storm column(s): ",
        paste(
          missing_columns,
          collapse = ", "
        ),
        "."
      ),
      call. = FALSE
    )
  }


  # ------------------------------------------------------------------
  # Settings
  # ------------------------------------------------------------------

  if (is.null(settings)) {

    settings <- attr(
      storms,
      "settings",
      exact = TRUE
    )

    if (is.null(settings)) {
      settings <- rf_settings()
    }
  }


  if (!inherits(
    settings,
    "rf_settings"
  )) {
    stop(
      "settings must be an object created by rf_settings().",
      call. = FALSE
    )
  }


  # ------------------------------------------------------------------
  # Temporal resolution
  # ------------------------------------------------------------------

  if (is.null(interval_min)) {

    interval_min <- attr(
      storms,
      "interval_min",
      exact = TRUE
    )
  }


  if (is.null(interval_min)) {
    stop(
      paste0(
        "interval_min was not supplied and could not be ",
        "obtained from the storm object."
      ),
      call. = FALSE
    )
  }


  if (
    length(interval_min) != 1 ||
    !is.numeric(interval_min) ||
    !is.finite(interval_min) ||
    interval_min <= 0
  ) {
    stop(
      "interval_min must be one positive finite value.",
      call. = FALSE
    )
  }


  # ------------------------------------------------------------------
  # Validate temporal-resolution compatibility
  # ------------------------------------------------------------------

  i30_intervals <-
    30 /
    interval_min


  if (
    abs(
      i30_intervals -
      round(
        i30_intervals
      )
    ) > 1e-10
  ) {
    stop(
      paste0(
        "interval_min must divide 30 minutes exactly because ",
        "EI30 requires a genuine 30-minute rainfall intensity."
      ),
      call. = FALSE
    )
  }


  duration_multiples <-
    settings$intensity_durations_min /
    interval_min


  incompatible_durations <-
    settings$intensity_durations_min[
      abs(
        duration_multiples -
          round(
            duration_multiples
          )
      ) > 1e-10
    ]


  if (
    length(
      incompatible_durations
    ) > 0
  ) {
    stop(
      paste0(
        "All intensity_durations_min values must be exact multiples ",
        "of interval_min. Incompatible duration(s): ",
        paste(
          incompatible_durations,
          collapse = ", "
        ),
        " minutes."
      ),
      call. = FALSE
    )
  }


  # ------------------------------------------------------------------
  # I30 is mandatory for EI30
  # ------------------------------------------------------------------

  if (
    !30 %in%
    settings$intensity_durations_min
  ) {
    stop(
      paste0(
        "intensity_durations_min must include 30 minutes ",
        "because EI30 requires maximum 30-minute intensity."
      ),
      call. = FALSE
    )
  }


  # ------------------------------------------------------------------
  # Handle zero storms
  # ------------------------------------------------------------------

  if (nrow(storms) == 0) {

    result <- data.frame(
      storm_id = integer(),
      event_start = as.POSIXct(
        character()
      ),
      event_end = as.POSIXct(
        character()
      ),
      duration_min = numeric(),
      precip_mm = numeric(),
      energy_mj_ha = numeric(),
      ei30 = numeric(),
      omit_precip_condition = logical(),
      omit_intensity_condition = logical(),
      omitted = logical(),
      erosive = logical()
    )


    for (
      duration in
      settings$intensity_durations_min
    ) {

      result[[
        paste0(
          "i",
          duration,
          "_mm_h"
        )
      ]] <- numeric()
    }


    class(result) <- c(
      "rf_events",
      "data.frame"
    )


    attr(
      result,
      "settings"
    ) <- settings


    attr(
      result,
      "interval_min"
    ) <- interval_min


    return(result)
  }


  # ------------------------------------------------------------------
  # Storm IDs
  # ------------------------------------------------------------------

  storm_ids <- sort(
    unique(
      storms$storm_id
    )
  )


  results <- vector(
    "list",
    length(storm_ids)
  )


  # ------------------------------------------------------------------
  # Process each storm
  # ------------------------------------------------------------------

  for (
    j in seq_along(
      storm_ids
    )
  ) {

    id <- storm_ids[j]


    x <- storms[
      storms$storm_id == id,
      c(
        "datetime",
        "precip_mm"
      ),
      drop = FALSE
    ]


    x <- x[
      order(
        x$datetime
      ),
      ,
      drop = FALSE
    ]


    # --------------------------------------------------------------
    # Validate storm rainfall data
    #
    # Storms produced by rf_identify_storms() are reconstructed on
    # the regular temporal grid before reaching this function.
    # --------------------------------------------------------------

    rf_validate_rainfall(
      x,
      expected_interval_min =
        interval_min
    )


    # --------------------------------------------------------------
    # Basic event statistics
    # --------------------------------------------------------------

    event_start <- min(
      x$datetime
    )


    event_end <- max(
      x$datetime
    )


    duration_min <-
      nrow(x) *
      interval_min


    total_precip_mm <- sum(
      x$precip_mm
    )


    # --------------------------------------------------------------
    # Rolling maximum intensities
    # --------------------------------------------------------------

    durations <-
      settings$intensity_durations_min


    intensities <- vapply(
      durations,
      function(duration) {

        rf_max_intensity(
          precip_mm =
            x$precip_mm,

          duration_min =
            duration,

          interval_min =
            interval_min
        )
      },
      numeric(1)
    )


    names(intensities) <-
      as.character(
        durations
      )


    # --------------------------------------------------------------
    # I30
    # --------------------------------------------------------------

    i30 <- unname(
      intensities["30"]
    )


    if (
      length(i30) != 1 ||
      is.na(i30)
    ) {
      stop(
        "A 30-minute intensity could not be calculated.",
        call. = FALSE
      )
    }


    # --------------------------------------------------------------
    # Intensity used by storm omission rule
    #
    # This is evaluated only when the intensity omission criterion
    # is enabled.
    # --------------------------------------------------------------

    if (settings$omit_intensity) {

      omission_duration <-
        as.character(
          settings$
            omit_intensity_duration_min
        )


      omission_intensity <-
        unname(
          intensities[
            omission_duration
          ]
        )


      if (
        length(omission_intensity) != 1 ||
        is.na(omission_intensity)
      ) {
        stop(
          paste0(
            "The required ",
            omission_duration,
            "-minute intensity was not calculated."
          ),
          call. = FALSE
        )
      }

    } else {

      omission_intensity <- NA_real_
    }


    # --------------------------------------------------------------
    # Rainfall kinetic energy and EI30
    #
    # RIST 3.99.10 was experimentally observed to assign
    # energy = 0 and EI30 = 0 when a storm consists of only
    # one rainfall record.
    #
    # Rfactor reproduces that behaviour only when requested
    # through:
    #
    # single_record_energy = "rist_zero"
    #
    # Otherwise the selected energy equation is applied normally.
    # --------------------------------------------------------------

    if (
      nrow(x) == 1 &&
      settings$single_record_energy ==
      "rist_zero"
    ) {

      energy_mj_ha <- 0

      ei30 <- 0

    } else {

      energy_mj_ha <-
        rf_calculate_energy(
          precip_mm =
            x$precip_mm,

          interval_min =
            interval_min,

          energy_equation =
            settings$
            energy_equation
        )


      ei30 <-
        energy_mj_ha *
        i30
    }


    # --------------------------------------------------------------
    # Omission criteria
    #
    # Each criterion is evaluated only when enabled.
    #
    # Disabled criteria are recorded as NA in the event table.
    #
    # Exact threshold values are retained because .rf_less_than()
    # implements the required strict "<" comparison.
    # --------------------------------------------------------------

    if (settings$omit_precip) {

      omit_precip_condition <-
        .rf_less_than(
          total_precip_mm,
          settings$omit_precip_below_mm
        )

    } else {

      omit_precip_condition <- NA
    }


    if (settings$omit_intensity) {

      omit_intensity_condition <-
        .rf_less_than(
          omission_intensity,
          settings$omit_intensity_below_mm_h
        )

    } else {

      omit_intensity_condition <- NA
    }


    # --------------------------------------------------------------
    # Build vector containing only SELECTED omission conditions
    # --------------------------------------------------------------

    selected_conditions <- logical()


    if (settings$omit_precip) {

      selected_conditions <- c(
        selected_conditions,
        omit_precip_condition
      )
    }


    if (settings$omit_intensity) {

      selected_conditions <- c(
        selected_conditions,
        omit_intensity_condition
      )
    }


    # --------------------------------------------------------------
    # ALL / ANY selected conditions
    #
    # If neither omission criterion is enabled, no storms are
    # omitted. This corresponds to the RIST "Include all storms"
    # option.
    # --------------------------------------------------------------

    if (length(selected_conditions) == 0) {

      omitted <- FALSE

    } else if (
      settings$omit_logic ==
      "all"
    ) {

      omitted <- all(
        selected_conditions
      )

    } else {

      omitted <- any(
        selected_conditions
      )
    }


    erosive <- !omitted


    # --------------------------------------------------------------
    # Base result row
    # --------------------------------------------------------------

    result_j <- data.frame(
      storm_id =
        as.integer(id),

      event_start =
        event_start,

      event_end =
        event_end,

      duration_min =
        duration_min,

      precip_mm =
        total_precip_mm,

      energy_mj_ha =
        energy_mj_ha,

      ei30 =
        ei30,

      omit_precip_condition =
        omit_precip_condition,

      omit_intensity_condition =
        omit_intensity_condition,

      omitted =
        omitted,

      erosive =
        erosive
    )


    # --------------------------------------------------------------
    # Add intensity columns
    # --------------------------------------------------------------

    for (
      k in seq_along(
        durations
      )
    ) {

      column_name <- paste0(
        "i",
        durations[k],
        "_mm_h"
      )


      result_j[[column_name]] <-
        intensities[k]
    }


    results[[j]] <- result_j
  }


  # ------------------------------------------------------------------
  # Combine all events
  # ------------------------------------------------------------------

  result <- do.call(
    rbind,
    results
  )


  rownames(result) <- NULL


  class(result) <- c(
    "rf_events",
    "data.frame"
  )


  attr(
    result,
    "settings"
  ) <- settings


  attr(
    result,
    "interval_min"
  ) <- interval_min


  result
}
