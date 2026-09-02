#' Calculate multi-year mean rainfall erosivity
#'
#' Calculates multi-year mean rainfall-runoff erosivity from monthly or
#' yearly R-factor values, typically produced by
#' [rf_calculate_rfactor()].
#'
#' @param rfactor A data frame containing monthly or yearly R-factor values.
#'   The input must contain a numeric `R` column and a `year` column.
#'   For `period = "monthly"`, a `month` column is also required.
#'
#' @param period Character string specifying the temporal scale of the input.
#'   One of `"yearly"` or `"monthly"`.
#'
#' @details
#' The function calculates the arithmetic multi-year mean of the available
#' R-factor values.
#'
#' Missing `R` values are excluded from the calculation. Genuine zero values
#' are retained because `R = 0` represents a valid period with no contributing
#' erosive events.
#'
#' For yearly input, the function calculates
#'
#' \deqn{
#' \bar{R} =
#' \frac{1}{n}
#' \sum_{y=1}^{n} R_y
#' }
#'
#' where \eqn{R_y} is the available rainfall erosivity for year \eqn{y} and
#' \eqn{n} is the number of non-missing yearly values.
#'
#' For monthly input, a separate multi-year mean is calculated for each
#' calendar month:
#'
#' \deqn{
#' \bar{R}_m =
#' \frac{1}{n_m}
#' \sum_{y=1}^{n_m} R_{y,m}
#' }
#'
#' where \eqn{R_{y,m}} is the available rainfall erosivity for month \eqn{m}
#' in year \eqn{y}, and \eqn{n_m} is the number of non-missing values
#' available for that calendar month.
#'
#' Missing years or month-year combinations are not generated or imputed.
#' An `NA` value is not included in either the mean or `n_years`.
#'
#' A value of `R = 0`, however, is included in the mean and in `n_years`.
#'
#' The function does not determine whether the underlying rainfall record is
#' climatologically complete. An available monthly or yearly R-factor value is
#' treated as valid unless the user removes it or marks it as `NA`.
#' Assessment of record completeness and representativeness remains the
#' responsibility of the user.
#'
#' The mean annual R-factor is calculated directly from yearly R-factor
#' values. It is not calculated by summing the twelve multi-year monthly
#' means. This distinction is important when data availability differs among
#' calendar months.
#'
#' @return
#' For `period = "yearly"`, a data frame with:
#'
#' \describe{
#'   \item{mean_R}{Multi-year mean annual rainfall erosivity.}
#'   \item{n_years}{Number of non-missing yearly R-factor values used.}
#' }
#'
#' For `period = "monthly"`, a data frame with:
#'
#' \describe{
#'   \item{month}{Calendar month as an integer from 1 to 12.}
#'   \item{mean_R}{Multi-year mean rainfall erosivity for that month.}
#'   \item{n_years}{Number of non-missing R-factor values used for that month.}
#' }
#'
#' Calendar months that are completely absent from the supplied input are not
#' generated.
#'
#' @examples
#' yearly <- data.frame(
#'   year = 2020:2024,
#'   R = c(
#'     820.4,
#'     910.2,
#'     NA,
#'     760.1,
#'     0
#'   )
#' )
#'
#' rf_calculate_mean_rfactor(
#'   yearly,
#'   period = "yearly"
#' )
#'
#' monthly <- data.frame(
#'   year = c(
#'     2020,
#'     2021,
#'     2022,
#'     2020,
#'     2021,
#'     2022
#'   ),
#'   month = c(
#'     1,
#'     1,
#'     1,
#'     2,
#'     2,
#'     2
#'   ),
#'   R = c(
#'     30,
#'     40,
#'     NA,
#'     50,
#'     0,
#'     70
#'   )
#' )
#'
#' rf_calculate_mean_rfactor(
#'   monthly,
#'   period = "monthly"
#' )
#'
#' @export
rf_calculate_mean_rfactor <- function(
    rfactor,
    period = c(
      "yearly",
      "monthly"
    )
) {

  # --------------------------------------------------------------------------
  # Match requested temporal scale
  # --------------------------------------------------------------------------

  period <- match.arg(
    period
  )


  # --------------------------------------------------------------------------
  # Validate input object
  # --------------------------------------------------------------------------

  if (
    !is.data.frame(
      rfactor
    )
  ) {
    stop(
      "rfactor must be a data frame.",
      call. = FALSE
    )
  }


  if (
    nrow(
      rfactor
    ) == 0
  ) {
    stop(
      "rfactor must contain at least one row.",
      call. = FALSE
    )
  }


  # --------------------------------------------------------------------------
  # Required columns
  # --------------------------------------------------------------------------

  required_columns <- c(
    "year",
    "R"
  )


  if (
    identical(
      period,
      "monthly"
    )
  ) {

    required_columns <- c(
      "year",
      "month",
      "R"
    )
  }


  missing_columns <- setdiff(
    required_columns,
    names(
      rfactor
    )
  )


  if (
    length(
      missing_columns
    ) > 0
  ) {
    stop(
      paste0(
        "rfactor is missing required column(s): ",
        paste(
          missing_columns,
          collapse = ", "
        ),
        "."
      ),
      call. = FALSE
    )
  }


  # --------------------------------------------------------------------------
  # Validate year
  # --------------------------------------------------------------------------

  if (
    !is.numeric(
      rfactor$year
    )
  ) {
    stop(
      "year must be numeric.",
      call. = FALSE
    )
  }


  if (
    any(
      is.na(
        rfactor$year
      ) |
      !is.finite(
        rfactor$year
      )
    )
  ) {
    stop(
      "year must not contain missing or non-finite values.",
      call. = FALSE
    )
  }


  if (
    any(
      abs(
        rfactor$year -
        round(
          rfactor$year
        )
      ) >
      1e-10
    )
  ) {
    stop(
      "year must contain integer-valued years.",
      call. = FALSE
    )
  }


  # --------------------------------------------------------------------------
  # Validate R
  # --------------------------------------------------------------------------

  if (
    !is.numeric(
      rfactor$R
    )
  ) {
    stop(
      "R must be numeric.",
      call. = FALSE
    )
  }


  if (
    any(
      is.infinite(
        rfactor$R
      )
    )
  ) {
    stop(
      "R must not contain infinite values.",
      call. = FALSE
    )
  }


  if (
    any(
      rfactor$R < 0,
      na.rm = TRUE
    )
  ) {
    stop(
      "R must not contain negative values.",
      call. = FALSE
    )
  }


  # --------------------------------------------------------------------------
  # Yearly calculation
  # --------------------------------------------------------------------------

  if (
    identical(
      period,
      "yearly"
    )
  ) {

    if (
      any(
        duplicated(
          rfactor$year
        )
      )
    ) {
      stop(
        paste0(
          "yearly input must contain at most one R-factor value ",
          "for each year."
        ),
        call. = FALSE
      )
    }


    available <- !is.na(
      rfactor$R
    )


    mean_R <- if (
      any(
        available
      )
    ) {

      mean(
        rfactor$R[
          available
        ]
      )

    } else {

      NA_real_
    }


    result <- data.frame(
      mean_R = mean_R,
      n_years = sum(
        available
      )
    )


    return(
      result
    )
  }


  # --------------------------------------------------------------------------
  # Validate month
  # --------------------------------------------------------------------------

  if (
    !is.numeric(
      rfactor$month
    )
  ) {
    stop(
      "month must be numeric.",
      call. = FALSE
    )
  }


  if (
    any(
      is.na(
        rfactor$month
      ) |
      !is.finite(
        rfactor$month
      )
    )
  ) {
    stop(
      "month must not contain missing or non-finite values.",
      call. = FALSE
    )
  }


  if (
    any(
      abs(
        rfactor$month -
        round(
          rfactor$month
        )
      ) >
      1e-10
    )
  ) {
    stop(
      "month must contain integer-valued calendar months.",
      call. = FALSE
    )
  }


  if (
    any(
      rfactor$month < 1 |
      rfactor$month > 12
    )
  ) {
    stop(
      "month must contain calendar months from 1 to 12.",
      call. = FALSE
    )
  }


  # --------------------------------------------------------------------------
  # Check for duplicated year-month combinations
  # --------------------------------------------------------------------------

  year_month <- paste(
    rfactor$year,
    rfactor$month,
    sep = "-"
  )


  if (
    any(
      duplicated(
        year_month
      )
    )
  ) {
    stop(
      paste0(
        "monthly input must contain at most one R-factor value ",
        "for each year-month combination."
      ),
      call. = FALSE
    )
  }


  # --------------------------------------------------------------------------
  # Calculate multi-year monthly means
  # --------------------------------------------------------------------------

  months <- sort(
    unique(
      as.integer(
        rfactor$month
      )
    )
  )


  result_list <- lapply(
    months,
    function(
    month_value
    ) {

      values <- rfactor$R[
        rfactor$month ==
          month_value
      ]


      available <- !is.na(
        values
      )


      mean_R <- if (
        any(
          available
        )
      ) {

        mean(
          values[
            available
          ]
        )

      } else {

        NA_real_
      }


      data.frame(
        month = month_value,
        mean_R = mean_R,
        n_years = sum(
          available
        )
      )
    }
  )


  result <- do.call(
    rbind,
    result_list
  )


  rownames(
    result
  ) <- NULL


  result
}
