.rf_datetime_formats <- c(
  "%d.%m.%Y %H:%M:%S",
  "%d.%m.%Y %H:%M",
  "%Y-%m-%d %H:%M:%S",
  "%Y-%m-%d %H:%M",
  "%d.%m.%Y",
  "%Y-%m-%d"
)


.rf_parse_datetime <- function(
    x,
    tz,
    ambiguous = "error",
    nonexistent = "error"
) {

  # The reader should supply raw datetime values as character.
  #
  # Convert defensively in case another character-like type is supplied.

  if (inherits(x, "POSIXt")) {
    stop(
      paste0(
        "Internal datetime parsing received an already parsed POSIXt ",
        "vector. Datetimes must be read as character before parsing."
      ),
      call. = FALSE
    )
  }


  x <- trimws(
    as.character(x)
  )


  result <- clock::date_time_parse(
    x = x,
    zone = tz,
    format = .rf_datetime_formats,
    ambiguous = ambiguous,
    nonexistent = nonexistent
  )


  if (anyNA(result)) {

    bad <- which(
      is.na(result)
    )

    example_bad <- utils::head(
      x[bad],
      5
    )

    stop(
      paste0(
        "One or more datetime values could not be parsed. ",
        "Accepted formats include: ",
        "'31.08.2025 23:30', ",
        "'31.08.2025 23:30:00', ",
        "'2025-08-31 23:30', ",
        "'2025-08-31 23:30:00', ",
        "'31.08.2025', and ",
        "'2025-08-31'. ",
        "Date-only values are interpreted as 00:00:00. ",
        "Example unparsed value(s): ",
        paste(
          shQuote(example_bad),
          collapse = ", "
        ),
        "."
      ),
      call. = FALSE
    )
  }


  result
}


#' Validate a rainfall time series
#'
#' Checks the structure, precipitation values, timestamp order, and
#' alignment with a declared base temporal resolution before rainfall
#' erosivity calculations are performed.
#'
#' The rainfall record does not need to be temporally complete.
#' Consecutive timestamps may be separated by gaps, provided each
#' separation is a positive whole multiple of
#' `expected_interval_min`.
#'
#' For example, with `expected_interval_min = 1`, timestamp differences
#' of 1, 2, 10, or 120 minutes are valid, whereas a difference of
#' 1.5 minutes is not.
#'
#' Validation does not assess whether a rainfall record is
#' climatologically complete. Missing time intervals, months, or years
#' are not filled, interpolated, estimated, or otherwise modified.
#'
#' The function requires:
#'
#' - a `POSIXct` `datetime` column;
#' - a numeric `precip_mm` column;
#' - no missing or duplicate timestamps;
#' - strictly increasing timestamps;
#' - finite, non-missing, non-negative precipitation values;
#' - timestamp differences compatible with the declared base interval.
#'
#' @param data A data frame containing columns named `datetime` and
#'   `precip_mm`.
#'
#' @param expected_interval_min Positive finite number giving the base
#'   temporal resolution of the rainfall record, in minutes.
#'   Consecutive timestamp differences may be positive whole multiples
#'   of this value. Default is `1`.
#'
#' @return `TRUE`, invisibly, when validation succeeds. An error is
#'   raised when the rainfall record is invalid.
#'
#' @examples
#' rainfall <- data.frame(
#'   datetime = as.POSIXct(
#'     c(
#'       "2025-01-01 00:00:00",
#'       "2025-01-01 00:01:00",
#'       "2025-01-01 00:10:00"
#'     ),
#'     tz = "UTC"
#'   ),
#'   precip_mm = c(0, 0.2, 0.1)
#' )
#'
#' # The nine-minute gap is allowed because it is a whole
#' # multiple of the one-minute base interval.
#' rf_validate_rainfall(
#'   rainfall,
#'   expected_interval_min = 1
#' )
#'
#' @export


rf_validate_rainfall <- function(
    data,
    expected_interval_min = 1
) {

  required <- c(
    "datetime",
    "precip_mm"
  )


  missing_columns <- setdiff(
    required,
    names(data)
  )


  if (length(missing_columns) > 0) {
    stop(
      paste0(
        "Missing required column(s): ",
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
    length(expected_interval_min) != 1 ||
    !is.numeric(expected_interval_min) ||
    !is.finite(expected_interval_min) ||
    expected_interval_min <= 0
  ) {
    stop(
      "expected_interval_min must be one positive finite value.",
      call. = FALSE
    )
  }


  if (
    !inherits(
      data$datetime,
      "POSIXct"
    )
  ) {
    stop(
      "datetime must be a POSIXct vector.",
      call. = FALSE
    )
  }


  if (!is.numeric(data$precip_mm)) {
    stop(
      "precip_mm must be numeric.",
      call. = FALSE
    )
  }


  if (anyNA(data$datetime)) {
    stop(
      "datetime contains missing values.",
      call. = FALSE
    )
  }


  if (anyNA(data$precip_mm)) {
    stop(
      "precip_mm contains missing values.",
      call. = FALSE
    )
  }


  if (any(!is.finite(data$precip_mm))) {
    stop(
      "precip_mm contains non-finite values.",
      call. = FALSE
    )
  }


  if (any(data$precip_mm < 0)) {
    stop(
      "Negative precipitation values are not allowed.",
      call. = FALSE
    )
  }


  if (anyDuplicated(data$datetime)) {
    stop(
      "Duplicate timestamps were detected.",
      call. = FALSE
    )
  }


  if (length(data$datetime) > 1) {

    intervals <- as.numeric(
      diff(
        data$datetime
      ),
      units = "mins"
    )


    # Timestamps must increase through time.
    #
    # rf_read_rainfall() sorts input rows before validation, but this
    # check also protects direct calls to rf_validate_rainfall().

    non_positive <- which(
      intervals <= 0
    )

    if (length(non_positive) > 0) {

      first_bad <- non_positive[1]

      stop(
        paste0(
          "Timestamps must be in strictly increasing order. ",
          "First invalid interval: ",
          format(
            data$datetime[first_bad],
            "%Y-%m-%d %H:%M:%S"
          ),
          " -> ",
          format(
            data$datetime[first_bad + 1],
            "%Y-%m-%d %H:%M:%S"
          ),
          "."
        ),
        call. = FALSE
      )
    }


    # Gaps are allowed.
    #
    # However, every timestamp must remain aligned with the declared
    # base temporal resolution. For a 1-minute rainfall series,
    # differences of 1, 2, 3, ... minutes are therefore valid.

    interval_multiples <-
      intervals /
      expected_interval_min


    bad <- which(
      abs(
        interval_multiples -
          round(
            interval_multiples
          )
      ) > 1e-8
    )


    if (length(bad) > 0) {

      first_bad <- bad[1]

      stop(
        paste0(
          "Timestamp spacing is not aligned with the expected ",
          expected_interval_min,
          "-minute temporal resolution. ",
          "First invalid interval: ",
          format(
            data$datetime[first_bad],
            "%Y-%m-%d %H:%M:%S"
          ),
          " -> ",
          format(
            data$datetime[first_bad + 1],
            "%Y-%m-%d %H:%M:%S"
          ),
          " (",
          intervals[first_bad],
          " minutes)."
        ),
        call. = FALSE
      )
    }
  }


  invisible(TRUE)
}


#' Read timestamped rainfall data
#'
#' Reads a delimited precipitation file and standardizes the selected
#' timestamp and precipitation columns to `datetime` and `precip_mm`.
#'
#' The input file does not need to contain every expected time step.
#' Sparse rainfall records and other gaps between observations are
#' accepted, provided timestamp differences remain compatible with the
#' declared base temporal resolution.
#'
#' The reader preserves the rainfall observations supplied by the user.
#' It does not insert missing time steps, interpolate precipitation,
#' estimate missing rainfall, or assess the climatological completeness
#' of the record.
#'
#' Accepted timestamp formats are:
#'
#' - `"DD.MM.YYYY HH:MM:SS"`;
#' - `"DD.MM.YYYY HH:MM"`;
#' - `"YYYY-MM-DD HH:MM:SS"`;
#' - `"YYYY-MM-DD HH:MM"`;
#' - `"DD.MM.YYYY"`;
#' - `"YYYY-MM-DD"`.
#'
#' Date-only values are interpreted as midnight (`00:00:00`) in the
#' time zone supplied through `tz`.
#'
#' Input rows are sorted chronologically when necessary. A warning is
#' issued when sorting changes the original row order.
#'
#' @param file Path to a delimited precipitation file readable by
#'   [data.table::fread()].
#'
#' @param datetime_col Name of the column containing rainfall
#'   timestamps.
#'
#' @param precip_col Name of the column containing precipitation
#'   amounts in millimetres.
#'
#' @param tz Time zone in which input timestamp values should be
#'   interpreted. Default is `"UTC"`.
#'
#' @param expected_interval_min Positive finite number giving the base
#'   temporal resolution of the rainfall record, in minutes.
#'   Default is `1`. Gaps consisting of positive whole multiples of
#'   this interval are allowed.
#'
#' @param ambiguous Handling of ambiguous local times, such as those
#'   occurring during a daylight-saving-time transition. Passed to
#'   [clock::date_time_parse()]. Default is `"error"`.
#'
#' @param nonexistent Handling of nonexistent local times, such as
#'   those occurring during a daylight-saving-time transition. Passed
#'   to [clock::date_time_parse()]. Default is `"error"`.
#'
#' @param validate Logical. If `TRUE`, the default, the standardized
#'   rainfall record is checked with [rf_validate_rainfall()]. If
#'   `FALSE`, this validation step is skipped.
#'
#' @return A data frame with:
#'
#' - `datetime`: parsed rainfall timestamps as `POSIXct`;
#' - `precip_mm`: precipitation amounts in millimetres.
#'
#' The returned object also stores the input time zone and declared
#' temporal resolution in the attributes `input_timezone` and
#' `expected_interval_min`.
#'
#' @examples
#' rainfall_file <- tempfile(fileext = ".csv")
#'
#' writeLines(
#'   c(
#'     "time,rain",
#'     "2025-01-01 00:00:00,0.0",
#'     "2025-01-01 00:05:00,0.4",
#'     "2025-01-01 00:10:00,0.2"
#'   ),
#'   rainfall_file
#' )
#'
#' rainfall <- rf_read_rainfall(
#'   rainfall_file,
#'   datetime_col = "time",
#'   precip_col = "rain",
#'   tz = "UTC",
#'   expected_interval_min = 5
#' )
#'
#' rainfall
#'
#' @export


rf_read_rainfall <- function(
    file,
    datetime_col,
    precip_col,
    tz = "UTC",
    expected_interval_min = 1,
    ambiguous = "error",
    nonexistent = "error",
    validate = TRUE
) {

  if (!file.exists(file)) {
    stop(
      paste0(
        "File does not exist: ",
        file
      ),
      call. = FALSE
    )
  }


  # Force the datetime column to remain character.
  #
  # This prevents fread() from automatically converting ISO-formatted
  # timestamps to POSIXct before Rfactor applies its own datetime parser
  # and user-specified time zone.

  datetime_col_class <- stats::setNames(
    "character",
    datetime_col
  )


  raw <- data.table::fread(
    file,
    na.strings = c(
      "",
      "NA",
      "NaN"
    ),
    colClasses = datetime_col_class
  )


  missing_columns <- setdiff(
    c(
      datetime_col,
      precip_col
    ),
    names(raw)
  )


  if (length(missing_columns) > 0) {
    stop(
      paste0(
        "Column(s) not found in input file: ",
        paste(
          missing_columns,
          collapse = ", "
        ),
        "."
      ),
      call. = FALSE
    )
  }


  datetime <- .rf_parse_datetime(
    raw[[datetime_col]],
    tz = tz,
    ambiguous = ambiguous,
    nonexistent = nonexistent
  )


  original_precip <- raw[[precip_col]]


  precip_mm <- suppressWarnings(
    as.numeric(
      original_precip
    )
  )


  conversion_failure <- (
    is.na(precip_mm) &
      !is.na(original_precip)
  )


  if (any(conversion_failure)) {
    stop(
      "One or more precipitation values could not be converted to numeric.",
      call. = FALSE
    )
  }


  input_order <- order(
    datetime
  )


  if (
    !identical(
      input_order,
      seq_along(datetime)
    )
  ) {
    warning(
      "Input timestamps were not ordered. Rows have been sorted.",
      call. = FALSE
    )
  }


  result <- data.frame(
    datetime =
      datetime[input_order],

    precip_mm =
      precip_mm[input_order]
  )


  attr(
    result,
    "input_timezone"
  ) <- tz


  attr(
    result,
    "expected_interval_min"
  ) <- expected_interval_min


  if (validate) {
    rf_validate_rainfall(
      result,
      expected_interval_min =
        expected_interval_min
    )
  }


  result
}
