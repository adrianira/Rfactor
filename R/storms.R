#' Identify independent rainfall events
#'
#' Splits a timestamped rainfall series into independent rainfall events
#' using the elapsed time between consecutive observations containing
#' positive rainfall.
#'
#' The input rainfall series does not need to contain every expected
#' time step. Sparse rainfall records and temporal gaps are allowed.
#'
#' In the fixed-interval storm-separation behaviour validated against
#' RIST 3.99.10, a new event begins when the elapsed time between
#' consecutive positive-rainfall observations is greater than
#' `storm_break_hours`.
#'
#' With the default setting of 6 hours:
#'
#' - a gap of 6 hours or less remains within the same event;
#' - a gap greater than 6 hours starts a new event.
#'
#' Any positive rainfall observation resets the storm-break clock,
#' regardless of its precipitation amount.
#'
#' The `storm_break_precip_mm` setting in [rf_settings()] is retained
#' as RIST configuration metadata but does not affect event separation
#' in the validated fixed-interval algorithm used by `Rfactor`.
#' Dedicated experiments with RIST 3.99.10 using 1-minute
#' fixed-interval rainfall data produced identical event grouping when
#' this setting was varied from 0 to 10 mm.
#'
#' @section Reconstruction of sparse events:
#'
#' After an event has been identified, a regular time grid is
#' reconstructed from its first through its last positive-rainfall
#' observation using `interval_min`.
#'
#' Observations supplied by the user, including explicitly recorded
#' zeros, are placed on this grid. Expected time positions absent from
#' the supplied record are assigned a precipitation amount of zero for
#' the calculation.
#'
#' This computational reconstruction preserves elapsed time for
#' rolling-intensity and energy calculations. An inserted zero does not
#' assert that the corresponding interval was observed and dry, and the
#' procedure does not estimate missing precipitation.
#'
#' Regular grids are reconstructed independently for each event.
#' Time steps lying between separate events are not generated.
#'
#' @param data A data frame containing `datetime` and `precip_mm`.
#'   The data are validated with [rf_validate_rainfall()].
#'
#' @param settings An `rf_settings` object created by [rf_settings()].
#'   Default settings are used when this argument is omitted.
#'
#' @param interval_min Positive finite number giving the base temporal
#'   resolution of the rainfall record, in minutes. If `NULL`, the
#'   `expected_interval_min` attribute stored by [rf_read_rainfall()] is
#'   used. If that metadata are not available, `interval_min` must be
#'   supplied explicitly.
#'
#' @return A data frame of class `rf_storms` containing:
#'
#' - `storm_id`: sequential event identifier;
#' - `datetime`: timestamps on the reconstructed within-event grid;
#' - `precip_mm`: precipitation amount at each grid position, in mm.
#'
#' The returned object also stores the calculation settings and temporal
#' resolution in the attributes `settings` and `interval_min`.
#'
#' An entirely dry rainfall series returns an empty `rf_storms` object.
#'
#' @examples
#' rainfall <- data.frame(
#'   datetime = as.POSIXct(
#'     c(
#'       "2025-01-01 00:00:00",
#'       "2025-01-01 03:00:00",
#'       "2025-01-01 07:00:00"
#'     ),
#'     tz = "UTC"
#'   ),
#'   precip_mm = c(5, 0.1, 5)
#' )
#'
#' # The positive rainfall at 03:00 resets the six-hour break clock,
#' # so all three observations belong to one event.
#' storms <- rf_identify_storms(
#'   rainfall,
#'   interval_min = 1
#' )
#'
#' storms
#'
#' @export


rf_identify_storms <- function(
    data,
    settings = rf_settings(),
    interval_min = NULL
) {

  # ------------------------------------------------------------------
  # Validate settings
  # ------------------------------------------------------------------

  if (!inherits(settings, "rf_settings")) {
    stop(
      "settings must be an object created by rf_settings().",
      call. = FALSE
    )
  }


  # ------------------------------------------------------------------
  # Store metadata before coercing to a plain data frame
  # ------------------------------------------------------------------

  stored_interval_min <- attr(
    data,
    "expected_interval_min",
    exact = TRUE
  )

  data <- as.data.frame(data)


  # ------------------------------------------------------------------
  # Determine temporal resolution
  # ------------------------------------------------------------------

  if (is.null(interval_min)) {

    interval_min <- stored_interval_min

    if (is.null(interval_min)) {
      stop(
        paste0(
          "interval_min must be supplied when the rainfall data do not ",
          "contain temporal-resolution metadata from rf_read_rainfall()."
        ),
        call. = FALSE
      )
    }
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
  # Validate rainfall series
  #
  # Temporal gaps are allowed. Validation only requires observations
  # to remain aligned with the declared base temporal resolution.
  # ------------------------------------------------------------------

  rf_validate_rainfall(
    data,
    expected_interval_min = interval_min
  )


  # ------------------------------------------------------------------
  # Locate observations containing positive rainfall
  # ------------------------------------------------------------------

  wet_indices <- which(
    data$precip_mm > 0
  )


  # ------------------------------------------------------------------
  # Handle an entirely dry series
  # ------------------------------------------------------------------

  if (length(wet_indices) == 0) {

    result <- data.frame(
      storm_id = integer(),
      datetime = data$datetime[FALSE],
      precip_mm = numeric()
    )

    class(result) <- c(
      "rf_storms",
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
  # Calculate gaps between consecutive positive-rainfall observations
  #
  # Experimental validation against RIST 3.99.10 with fixed-interval
  # 1-minute rainfall data showed:
  #
  # gap <= storm_break_hours  -> same storm
  # gap >  storm_break_hours  -> new storm
  #
  # Any positive rainfall observation resets the break clock.
  #
  # Dedicated experiments using storm_break_precip_mm values from
  # 0 to 10 mm produced identical storm grouping. Therefore
  # storm_break_precip_mm is retained as RIST configuration metadata
  # and is not used by this validated fixed-interval algorithm.
  #
  # Use seconds here rather than decimal hours to make the exact
  # storm-break boundary explicit.
  # ------------------------------------------------------------------

  wet_times <- data$datetime[
    wet_indices
  ]


  if (length(wet_times) == 1) {

    gap_seconds <- Inf

  } else {

    gap_seconds <- c(
      Inf,
      as.numeric(
        difftime(
          wet_times[-1],
          wet_times[-length(wet_times)],
          units = "secs"
        )
      )
    )
  }


  break_seconds <-
    settings$storm_break_hours *
    60 *
    60


  # ------------------------------------------------------------------
  # Identify the first wet observation of each new storm
  # ------------------------------------------------------------------

  new_storm <-
    gap_seconds >
    break_seconds


  storm_id_wet <- cumsum(
    new_storm
  )


  # ------------------------------------------------------------------
  # Determine first and last positive-rainfall time of each storm
  # ------------------------------------------------------------------

  storm_ids <- unique(
    storm_id_wet
  )


  storm_ranges <- lapply(
    storm_ids,
    function(id) {

      times_for_storm <- wet_times[
        storm_id_wet == id
      ]

      list(
        start = min(
          times_for_storm
        ),
        end = max(
          times_for_storm
        )
      )
    }
  )


  # ------------------------------------------------------------------
  # Build output
  #
  # A complete regular time grid is reconstructed independently for
  # each storm.
  #
  # Existing rainfall observations are preserved exactly.
  #
  # Expected time steps absent from the supplied input are assigned
  # zero recorded rainfall. This preserves elapsed time for rolling
  # intensity calculations without requiring sparse source files to
  # contain millions of explicit zero-rainfall rows.
  # ------------------------------------------------------------------

  result_list <- vector(
    "list",
    length(
      storm_ranges
    )
  )


  for (i in seq_along(storm_ranges)) {

    start_time <-
      storm_ranges[[i]]$start

    end_time <-
      storm_ranges[[i]]$end


    # seq.POSIXt interprets a numeric `by` value as seconds.

    storm_grid <- seq(
      from = start_time,
      to = end_time,
      by = interval_min * 60
    )


    # Retain any observations supplied by the user within the storm
    # boundaries. These may include both positive rainfall and
    # explicitly recorded zero rainfall.

    in_storm <- (
      data$datetime >= start_time &
        data$datetime <= end_time
    )


    observed <- data[
      in_storm,
      c(
        "datetime",
        "precip_mm"
      ),
      drop = FALSE
    ]


    # Start with zero recorded rainfall for every expected interval.

    storm_precip <- rep(
      0,
      length(
        storm_grid
      )
    )


    # Match supplied observations to the reconstructed regular grid.

    matched <- match(
      as.numeric(
        observed$datetime
      ),
      as.numeric(
        storm_grid
      )
    )


    if (anyNA(matched)) {
      stop(
        paste0(
          "One or more rainfall observations could not be aligned ",
          "with the reconstructed storm time grid."
        ),
        call. = FALSE
      )
    }


    storm_precip[
      matched
    ] <- observed$precip_mm


    result_list[[i]] <- data.frame(
      storm_id = rep(
        as.integer(i),
        length(
          storm_grid
        )
      ),
      datetime = storm_grid,
      precip_mm = storm_precip
    )
  }


  result <- do.call(
    rbind,
    result_list
  )


  rownames(result) <- NULL


  # ------------------------------------------------------------------
  # Class and metadata
  # ------------------------------------------------------------------

  class(result) <- c(
    "rf_storms",
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
