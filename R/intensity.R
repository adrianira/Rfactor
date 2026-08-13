#' Calculate maximum rolling rainfall intensity
#'
#' Calculates the maximum rainfall intensity over a continuous rolling
#' time window.
#'
#' The calculation evaluates all possible consecutive windows represented
#' by the rainfall vector; windows are not restricted to fixed clock
#' boundaries. For example, with 1-minute rainfall data and
#' `duration_min = 30`, the maximum depth occurring in any consecutive
#' 30-minute window is used.
#'
#' The requested duration must be an exact multiple of the rainfall
#' interval. Thus, 10-minute rainfall data can be used directly for
#' 10-, 20-, 30-, or 60-minute intensities, but not for a true
#' 5- or 15-minute intensity.
#'
#' `precip_mm` is assumed to represent consecutive observations on a
#' regular time grid with spacing `interval_min`. The function does not
#' contain timestamps and therefore cannot detect gaps. In the normal
#' `Rfactor` workflow, [rf_identify_storms()] reconstructs the regular
#' within-event grid before rainfall intensities are calculated.
#'
#' Partial rolling windows are permitted. When fewer observations are
#' available than required for a complete window, the available rainfall
#' depth is divided by the full requested duration. For example, 10 mm
#' falling during a 10-minute event gives a 30-minute intensity of
#' 20 mm/h:
#'
#' \deqn{I_{30} = 10 \times 60 / 30 = 20\ {\rm mm/h}}
#'
#' This treatment reproduces the behaviour observed during validation
#' against RIST 3.99.10 fixed-interval rainfall input.
#'
#' @param precip_mm Numeric vector of precipitation depths, in mm, for
#'   consecutive regular rainfall intervals. Values must be finite,
#'   non-missing, and non-negative.
#'
#' @param duration_min Positive finite number giving the duration of the
#'   rolling intensity window, in minutes. It must be an exact multiple
#'   of `interval_min`. Default is `30`.
#'
#' @param interval_min Positive finite number giving the temporal
#'   resolution represented by each element of `precip_mm`, in minutes.
#'   Default is `1`.
#'
#' @return The maximum rolling rainfall intensity, in mm/h.
#'
#' @examples
#' # Thirty minutes of rainfall at 1 mm/min:
#' # 30 mm in 30 minutes = 60 mm/h
#' rf_max_intensity(
#'   precip_mm = rep(1, 30),
#'   duration_min = 30,
#'   interval_min = 1
#' )
#'
#' # Ten-minute rainfall data can be used directly for I30.
#' rf_max_intensity(
#'   precip_mm = c(1, 2, 3),
#'   duration_min = 30,
#'   interval_min = 10
#' )
#'
#' # A rainfall event shorter than the requested window uses a
#' # partial window. Ten mm in a 10-minute event gives I30 = 20 mm/h.
#' rf_max_intensity(
#'   precip_mm = rep(1, 10),
#'   duration_min = 30,
#'   interval_min = 1
#' )
#'
#' @export


rf_max_intensity <- function(
    precip_mm,
    duration_min = 30,
    interval_min = 1
) {

  if (!is.numeric(precip_mm)) {
    stop(
      "precip_mm must be numeric.",
      call. = FALSE
    )
  }

  if (length(precip_mm) == 0) {
    stop(
      "precip_mm must contain at least one value.",
      call. = FALSE
    )
  }

  if (anyNA(precip_mm)) {
    stop(
      "precip_mm contains missing values.",
      call. = FALSE
    )
  }

  if (any(!is.finite(precip_mm))) {
    stop(
      "precip_mm contains non-finite values.",
      call. = FALSE
    )
  }

  if (any(precip_mm < 0)) {
    stop(
      "precip_mm cannot contain negative values.",
      call. = FALSE
    )
  }

  if (
    length(duration_min) != 1 ||
    !is.finite(duration_min) ||
    duration_min <= 0
  ) {
    stop(
      "duration_min must be one positive finite value.",
      call. = FALSE
    )
  }

  if (
    length(interval_min) != 1 ||
    !is.finite(interval_min) ||
    interval_min <= 0
  ) {
    stop(
      "interval_min must be one positive finite value.",
      call. = FALSE
    )
  }

  n_intervals <- duration_min /
    interval_min

  if (
    abs(
      n_intervals -
      round(n_intervals)
    ) > 1e-10
  ) {
    stop(
      paste0(
        "duration_min must be an exact multiple ",
        "of interval_min."
      ),
      call. = FALSE
    )
  }

  n_intervals <- as.integer(
    round(n_intervals)
  )

  rolling_depth <- data.table::frollsum(
    precip_mm,
    n = n_intervals,
    align = "right",
    na.rm = FALSE,
    partial = TRUE
  )

  maximum_depth <- max(
    rolling_depth
  )

  maximum_depth *
    60 /
    duration_min
}
