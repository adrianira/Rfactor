test_that(
  "rf_calculate_rfactor aggregates yearly EI30",
  {

    events <- data.frame(
      event_start = as.POSIXct(
        c(
          "2025-01-10 12:00:00",
          "2025-01-20 12:00:00",
          "2025-02-10 12:00:00",
          "2026-01-10 12:00:00"
        ),
        tz = "UTC"
      ),
      ei30 = c(
        10,
        20,
        30,
        40
      ),
      erosive = c(
        TRUE,
        FALSE,
        TRUE,
        TRUE
      )
    )

    result <- rf_calculate_rfactor(
      events,
      period = "yearly"
    )

    expect_equal(
      result$year,
      c(
        2025L,
        2026L
      )
    )

    expect_equal(
      result$R,
      c(
        40,
        40
      )
    )

    expect_equal(
      result$n_events,
      c(
        2L,
        1L
      )
    )
  }
)


test_that(
  "rf_calculate_rfactor aggregates monthly EI30",
  {

    events <- data.frame(
      event_start = as.POSIXct(
        c(
          "2025-01-10 12:00:00",
          "2025-01-20 12:00:00",
          "2025-02-10 12:00:00",
          "2026-01-10 12:00:00"
        ),
        tz = "UTC"
      ),
      ei30 = c(
        10,
        20,
        30,
        40
      ),
      erosive = c(
        TRUE,
        FALSE,
        TRUE,
        TRUE
      )
    )

    result <- rf_calculate_rfactor(
      events,
      period = "monthly"
    )

    expect_equal(
      result$year,
      c(
        2025L,
        2025L,
        2026L
      )
    )

    expect_equal(
      result$month,
      c(
        1L,
        2L,
        1L
      )
    )

    expect_equal(
      result$R,
      c(
        10,
        30,
        40
      )
    )
  }
)


test_that(
  "monthly R-factor results are ordered chronologically",
  {

    events <- data.frame(
      event_start = as.POSIXct(
        c(
          "2025-10-01 00:00:00",
          "2024-12-01 00:00:00",
          "2025-04-01 00:00:00",
          "2024-02-01 00:00:00"
        ),
        tz = "UTC"
      ),
      ei30 = c(
        10,
        20,
        30,
        40
      ),
      erosive = c(
        TRUE,
        TRUE,
        TRUE,
        TRUE
      )
    )

    result <- rf_calculate_rfactor(
      events,
      period = "monthly"
    )

    expect_equal(
      result$year,
      c(
        2024L,
        2024L,
        2025L,
        2025L
      )
    )

    expect_equal(
      result$month,
      c(
        2L,
        12L,
        4L,
        10L
      )
    )
  }
)


test_that(
  "yearly R-factor results are ordered chronologically",
  {

    events <- data.frame(
      event_start = as.POSIXct(
        c(
          "2025-01-01 00:00:00",
          "1991-01-01 00:00:00",
          "2010-01-01 00:00:00"
        ),
        tz = "UTC"
      ),
      ei30 = c(
        10,
        20,
        30
      ),
      erosive = c(
        TRUE,
        TRUE,
        TRUE
      )
    )

    result <- rf_calculate_rfactor(
      events,
      period = "yearly"
    )

    expect_equal(
      result$year,
      c(
        1991L,
        2010L,
        2025L
      )
    )
  }
)


test_that(
  "event aggregation validates required event values",
  {

    base_events <- data.frame(
      event_start = as.POSIXct(
        "2025-01-01 00:00:00",
        tz = "UTC"
      ),
      ei30 = 10,
      erosive = TRUE
    )


    x <- base_events
    x$erosive <- 1

    expect_error(
      rf_calculate_rfactor(x),
      "erosive must be logical"
    )


    x <- base_events
    x$erosive <- NA

    expect_error(
      rf_calculate_rfactor(x),
      "erosive contains missing"
    )


    x <- base_events
    x$ei30 <- -1

    expect_error(
      rf_calculate_rfactor(x),
      "ei30 cannot contain negative"
    )
  }
)
