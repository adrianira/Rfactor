#' Aggregate event rainfall erosivity by calendar period
#'
#' Aggregates event EI30 erosivity into monthly or yearly calendar
#' totals.
#'
#' Only events with `erosive == TRUE` and a non-missing `ei30` value
#' contribute to the erosivity total. Non-erosive or omitted events
#' contribute zero.
#'
#' Calendar periods represented by at least one event are retained
#' even when none of their events contribute erosivity. Such periods
#' are returned with `R = 0` and `n_events = 0`.
#'
#' Calendar months or years that are completely absent from the
#' supplied event data are not created, filled, or imputed.
#'
#' @section Interpretation of R:
#'
#' The `R` column returned by this function is the sum of contributing
#' event EI30 values for the requested calendar period:
#'
#' \deqn{
#' R_p = \sum_{j \in p} EI_{30,j}
#' }
#'
#' where the sum includes only events classified as erosive and having
#' a non-missing EI30 value.
#'
#' A yearly value returned by this function represents the rainfall
#' erosivity calculated from the available events for that calendar
#' year. It is not, by itself, the climatological long-term mean annual
#' rainfall-runoff erosivity factor (R-factor) used by USLE or RUSLE.
#'
#' Calculation of a representative long-term R-factor requires an
#' appropriate multi-year rainfall record and assessment of record
#' completeness and representativeness outside this aggregation
#' function.
#'
#' @section Calendar assignment:
#'
#' Each event is assigned to the calendar month or year containing its
#' `event_start` timestamp. Consequently, an event that crosses a month
#' or year boundary is assigned in full to the period in which it
#' begins.
#'
#' @param events A data frame containing at least:
#'
#' - `event_start`: event starting time as `POSIXct`;
#' - `ei30`: event EI30 erosivity in MJ mm/(ha h);
#' - `erosive`: logical event classification.
#'
#' Normally this is an `rf_events` object returned by
#' [rf_calculate_ei30()].
#'
#' @param period Calendar aggregation period. Either `"yearly"` or
#'   `"monthly"`. Default is `"yearly"`.
#'
#' @return A data frame ordered chronologically.
#'
#' For `period = "yearly"`, the columns are:
#'
#' - `year`: calendar year;
#' - `R`: sum of contributing event EI30 values for that year, in
#'   MJ mm/(ha h);
#' - `n_events`: number of erosive events with non-missing EI30 that
#'   contribute to the total.
#'
#' For `period = "monthly"`, the result additionally contains:
#'
#' - `month`: calendar month as an integer from 1 to 12.
#'
#' An empty event table returns an empty data frame with the
#' corresponding yearly or monthly structure.
#'
#' @examples
#' events <- data.frame(
#'   event_start = as.POSIXct(
#'     c(
#'       "2025-05-10 12:00:00",
#'       "2025-05-20 15:00:00",
#'       "2025-06-05 09:00:00"
#'     ),
#'     tz = "UTC"
#'   ),
#'   ei30 = c(
#'     100,
#'     25,
#'     40
#'   ),
#'   erosive = c(
#'     TRUE,
#'     FALSE,
#'     FALSE
#'   )
#' )
#'
#' # May is represented with R = 100.
#' # June is represented with R = 0 because it contains rainfall
#' # events but none classified as erosive.
#' rf_calculate_rfactor(
#'   events,
#'   period = "monthly"
#' )
#'
#' rf_calculate_rfactor(
#'   events,
#'   period = "yearly"
#' )
#'
#' @export


rf_calculate_rfactor <- function(
    events,
    period = c(
      "yearly",
      "monthly"
    )
) {

  period <- match.arg(
    period
  )


  required <- c(
    "event_start",
    "ei30",
    "erosive"
  )


  missing_columns <- setdiff(
    required,
    names(events)
  )


  if (length(missing_columns) > 0) {
    stop(
      paste0(
        "Missing required event column(s): ",
        paste(
          missing_columns,
          collapse = ", "
        ),
        "."
      ),
      call. = FALSE
    )
  }


  if (
    !inherits(
      events$event_start,
      "POSIXct"
    )
  ) {
    stop(
      "event_start must be POSIXct.",
      call. = FALSE
    )
  }


  if (!is.numeric(events$ei30)) {
    stop(
      "ei30 must be numeric.",
      call. = FALSE
    )
  }


  if (anyNA(events$event_start)) {
    stop(
      "event_start contains missing values.",
      call. = FALSE
    )
  }


  if (!is.logical(events$erosive)) {
    stop(
      "erosive must be logical.",
      call. = FALSE
    )
  }


  if (anyNA(events$erosive)) {
    stop(
      "erosive contains missing values.",
      call. = FALSE
    )
  }


  available_ei30 <- !is.na(
    events$ei30
  )


  if (
    any(
      !is.finite(
        events$ei30[
          available_ei30
        ]
      )
    )
  ) {
    stop(
      "ei30 contains non-finite values.",
      call. = FALSE
    )
  }


  if (
    any(
      events$ei30[
        available_ei30
      ] < 0
    )
  ) {
    stop(
      "ei30 cannot contain negative values.",
      call. = FALSE
    )
  }


  # ------------------------------------------------------------------
  # Handle an empty event table
  # ------------------------------------------------------------------

  if (nrow(events) == 0) {

    if (period == "yearly") {

      return(
        data.frame(
          year = integer(),
          R = numeric(),
          n_events = integer()
        )
      )

    } else {

      return(
        data.frame(
          year = integer(),
          month = integer(),
          R = numeric(),
          n_events = integer()
        )
      )
    }
  }


  # ------------------------------------------------------------------
  # Determine which events contribute to R
  #
  # All events remain in the working table so that periods containing
  # rainfall events but no erosive events are still represented.
  #
  # Only erosive events with a non-missing EI30 contribute to R and
  # to n_events.
  # ------------------------------------------------------------------

  contributes <- (
    events$erosive %in% TRUE &
      !is.na(events$ei30)
  )


  r_contribution <- rep(
    0,
    nrow(events)
  )


  r_contribution[
    contributes
  ] <- events$ei30[
    contributes
  ]


  event_contribution <- as.integer(
    contributes
  )


  # ------------------------------------------------------------------
  # Calendar components
  # ------------------------------------------------------------------

  year <- as.integer(
    format(
      events$event_start,
      "%Y"
    )
  )


  # ------------------------------------------------------------------
  # Yearly aggregation
  # ------------------------------------------------------------------

  if (period == "yearly") {

    working <- data.frame(
      year = year,
      R = r_contribution,
      n_events = event_contribution
    )


    r_sums <- stats::aggregate(
      R ~ year,
      data = working,
      FUN = sum
    )


    event_counts <- stats::aggregate(
      n_events ~ year,
      data = working,
      FUN = sum
    )


    result <- merge(
      r_sums,
      event_counts,
      by = "year",
      sort = TRUE
    )


    # ------------------------------------------------------------------
    # Monthly aggregation
    # ------------------------------------------------------------------

  } else {

    month <- as.integer(
      format(
        events$event_start,
        "%m"
      )
    )


    working <- data.frame(
      year = year,
      month = month,
      R = r_contribution,
      n_events = event_contribution
    )


    r_sums <- stats::aggregate(
      R ~ year + month,
      data = working,
      FUN = sum
    )


    event_counts <- stats::aggregate(
      n_events ~ year + month,
      data = working,
      FUN = sum
    )


    result <- merge(
      r_sums,
      event_counts,
      by = c(
        "year",
        "month"
      ),
      sort = TRUE
    )
  }


  # ------------------------------------------------------------------
  # Chronological ordering
  # ------------------------------------------------------------------

  if (period == "yearly") {

    result <- result[
      order(
        result$year
      ),
      ,
      drop = FALSE
    ]

  } else {

    result <- result[
      order(
        result$year,
        result$month
      ),
      ,
      drop = FALSE
    ]
  }


  rownames(result) <- NULL

  result
}
