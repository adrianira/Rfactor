test_that(
  "rf_read_rainfall accepts all supported datetime formats",
  {

    file <- tempfile(
      fileext = ".csv"
    )

    writeLines(
      c(
        "date,precip",
        "31.08.2025 23:58,0.0",
        "31.08.2025 23:59:00,0.1",
        "01.09.2025,0.2",
        "2025-09-01 00:01,0.3",
        "2025-09-01 00:02:00,0.0",
        "2025-09-02,0.4"
      ),
      file
    )

    x <- rf_read_rainfall(
      file = file,
      datetime_col = "date",
      precip_col = "precip",
      tz = "UTC",
      expected_interval_min = 1
    )

    expect_equal(
      nrow(x),
      6
    )

    expect_equal(
      x$precip_mm,
      c(
        0,
        0.1,
        0.2,
        0.3,
        0,
        0.4
      )
    )

    expect_equal(
      format(
        x$datetime,
        tz = "UTC",
        format = "%Y-%m-%d %H:%M:%S"
      ),
      c(
        "2025-08-31 23:58:00",
        "2025-08-31 23:59:00",
        "2025-09-01 00:00:00",
        "2025-09-01 00:01:00",
        "2025-09-01 00:02:00",
        "2025-09-02 00:00:00"
      )
    )
  }
)


test_that(
  "rf_validate_rainfall accepts temporal gaps",
  {

    x <- data.frame(
      datetime = as.POSIXct(
        c(
          "2025-01-01 00:00:00",
          "2025-01-01 00:01:00",
          "2025-01-01 00:03:00",
          "2025-01-01 02:15:00"
        ),
        tz = "UTC"
      ),
      precip_mm = c(
        0,
        0.1,
        0,
        0.2
      )
    )

    expect_true(
      rf_validate_rainfall(
        x,
        expected_interval_min = 1
      )
    )
  }
)


test_that(
  "rf_validate_rainfall rejects timestamps off the expected temporal grid",
  {

    x <- data.frame(
      datetime = as.POSIXct(
        c(
          "2025-01-01 00:00:00",
          "2025-01-01 00:01:30",
          "2025-01-01 00:03:00"
        ),
        tz = "UTC"
      ),
      precip_mm = c(
        0,
        0.1,
        0
      )
    )

    expect_error(
      rf_validate_rainfall(
        x,
        expected_interval_min = 1
      ),
      "not aligned"
    )
  }
)


test_that(
  "rf_validate_rainfall rejects negative precipitation",
  {

    x <- data.frame(
      datetime = as.POSIXct(
        c(
          "2025-01-01 00:00:00",
          "2025-01-01 00:01:00"
        ),
        tz = "UTC"
      ),
      precip_mm = c(
        0,
        -0.1
      )
    )

    expect_error(
      rf_validate_rainfall(x),
      "Negative precipitation"
    )
  }
)


test_that(
  "rf_read_rainfall correctly reads ISO datetimes at midnight",
  {

    file <- tempfile(
      fileext = ".csv"
    )

    writeLines(
      c(
        "datetime,precip",
        "2025-07-01 00:00:00,0",
        "2025-07-01 00:01:00,0.1",
        "2025-07-01 00:02:00,0"
      ),
      file
    )

    x <- rf_read_rainfall(
      file = file,
      datetime_col = "datetime",
      precip_col = "precip",
      tz = "UTC",
      expected_interval_min = 1
    )

    expect_equal(
      nrow(x),
      3
    )

    expect_equal(
      format(
        x$datetime,
        format = "%Y-%m-%d %H:%M:%S",
        tz = "UTC"
      ),
      c(
        "2025-07-01 00:00:00",
        "2025-07-01 00:01:00",
        "2025-07-01 00:02:00"
      )
    )
  }
)


test_that(
  "rf_read_rainfall interprets date-only timestamps as midnight",
  {

    file <- tempfile(
      fileext = ".csv"
    )

    writeLines(
      c(
        "datetime,precip",
        "2025-06-19 23:59:00,0.28",
        "2025-06-20,0.28",
        "2025-06-20 00:01:00,0.28"
      ),
      file
    )

    x <- rf_read_rainfall(
      file = file,
      datetime_col = "datetime",
      precip_col = "precip",
      tz = "UTC",
      expected_interval_min = 1
    )

    expect_equal(
      format(
        x$datetime,
        format = "%Y-%m-%d %H:%M:%S",
        tz = "UTC"
      ),
      c(
        "2025-06-19 23:59:00",
        "2025-06-20 00:00:00",
        "2025-06-20 00:01:00"
      )
    )

    expect_equal(
      x$precip_mm,
      c(
        0.28,
        0.28,
        0.28
      )
    )
  }
)


test_that(
  "rf_read_rainfall respects the supplied time zone",
  {

    file <- tempfile(
      fileext = ".csv"
    )

    writeLines(
      c(
        "datetime,precip",
        "2025-07-01 12:00:00,0",
        "2025-07-01 12:01:00,0"
      ),
      file
    )

    x <- rf_read_rainfall(
      file = file,
      datetime_col = "datetime",
      precip_col = "precip",
      tz = "Europe/Bucharest",
      expected_interval_min = 1
    )

    expect_equal(
      attr(
        x$datetime,
        "tzone"
      ),
      "Europe/Bucharest"
    )

    expect_equal(
      format(
        x$datetime,
        format = "%Y-%m-%d %H:%M:%S",
        tz = "Europe/Bucharest"
      ),
      c(
        "2025-07-01 12:00:00",
        "2025-07-01 12:01:00"
      )
    )
  }
)
