make_test_rain <- function(n) {

  x <- data.frame(
    datetime = as.POSIXct(
      "2025-01-01 00:00:00",
      tz = "UTC"
    ) +
      seq(
        from = 0,
        by = 60,
        length.out = n
      ),

    precip_mm = rep(
      0,
      n
    )
  )


  attr(
    x,
    "expected_interval_min"
  ) <- 1


  x
}


test_that(
  "rainfall less than six hours apart remains one storm",
  {

    x <- make_test_rain(360)

    # First rainfall: 00:00
    x$precip_mm[1] <- 5

    # Second rainfall: 05:59
    x$precip_mm[360] <- 5

    storms <- rf_identify_storms(x)

    expect_equal(
      length(
        unique(
          storms$storm_id
        )
      ),
      1
    )

    expect_equal(
      sum(
        storms$precip_mm
      ),
      10
    )
  }
)


test_that(
  "rainfall exactly six hours apart remains one storm",
  {

    x <- make_test_rain(361)

    # First rainfall: 00:00
    x$precip_mm[1] <- 5

    # Second rainfall: 06:00
    x$precip_mm[361] <- 5

    storms <- rf_identify_storms(x)

    expect_equal(
      length(
        unique(
          storms$storm_id
        )
      ),
      1
    )

    expect_equal(
      sum(
        storms$precip_mm
      ),
      10
    )
  }
)


test_that(
  "rainfall more than six hours apart forms separate storms",
  {

    x <- make_test_rain(362)

    # First rainfall: 00:00
    x$precip_mm[1] <- 5

    # Second rainfall: 06:01
    x$precip_mm[362] <- 5

    storms <- rf_identify_storms(x)

    expect_equal(
      unique(
        storms$storm_id
      ),
      c(
        1L,
        2L
      )
    )

    expect_equal(
      sum(
        storms$precip_mm[
          storms$storm_id == 1
        ]
      ),
      5
    )

    expect_equal(
      sum(
        storms$precip_mm[
          storms$storm_id == 2
        ]
      ),
      5
    )
  }
)


test_that(
  "positive intermediate rainfall resets the storm-break clock",
  {

    x <- make_test_rain(362)

    # First rainfall: 00:00
    x$precip_mm[1] <- 5

    # Tiny intermediate rainfall: 03:00
    x$precip_mm[181] <- 0.001

    # Final rainfall: 06:01
    x$precip_mm[362] <- 5

    storms <- rf_identify_storms(x)

    expect_equal(
      length(
        unique(
          storms$storm_id
        )
      ),
      1
    )

    expect_equal(
      sum(
        storms$precip_mm
      ),
      10.001,
      tolerance = 1e-12
    )
  }
)


test_that(
  "storm-break duration is customizable",
  {

    # Use a 1-hour storm-break setting.
    #
    # Rainfall at:
    #
    # 00:00
    # 01:00  -> exactly 1 hour later: same storm
    # 02:01  -> 1 hour 1 minute later: new storm

    x <- make_test_rain(
      122
    )

    x$precip_mm[1] <- 5

    x$precip_mm[61] <- 5

    x$precip_mm[122] <- 5


    settings <- rf_settings(
      storm_break_hours = 1
    )


    storms <- rf_identify_storms(
      x,
      settings = settings
    )


    expect_equal(
      unique(
        storms$storm_id
      ),
      c(
        1L,
        2L
      )
    )


    expect_equal(
      sum(
        storms$precip_mm[
          storms$storm_id == 1
        ]
      ),
      10
    )


    expect_equal(
      sum(
        storms$precip_mm[
          storms$storm_id == 2
        ]
      ),
      5
    )
  }
)


test_that(
  "storm-break precipitation setting does not alter fixed-interval grouping",
  {

    # Dedicated RIST 3.99.10 fixed-interval experiments showed
    # identical storm grouping when the precipitation setting was
    # changed across:
    #
    # 0, 0.20, 1.27, 2.00, and 10.00 mm.
    #
    # Therefore this setting is retained as RIST configuration
    # metadata and must not affect the validated fixed-interval
    # storm-separation algorithm.

    thresholds <- c(
      0,
      0.20,
      1.27,
      2.00,
      10.00
    )


    # --------------------------------------------------------------
    # Case A:
    #
    # No intermediate positive rainfall.
    #
    # 00:00 -> 06:01
    #
    # Gap > 6 hours, so this must always produce two storms.
    # --------------------------------------------------------------

    x_split <- make_test_rain(
      362
    )

    x_split$precip_mm[1] <- 5

    x_split$precip_mm[362] <- 5


    # --------------------------------------------------------------
    # Case B:
    #
    # Tiny positive rainfall at 03:00 resets the break clock.
    #
    # 00:00 -> 03:00 -> 06:01
    #
    # Both positive-rainfall gaps are <= 6 hours, so this must
    # always remain one storm.
    # --------------------------------------------------------------

    x_reset <- make_test_rain(
      362
    )

    x_reset$precip_mm[1] <- 5

    x_reset$precip_mm[181] <- 0.001

    x_reset$precip_mm[362] <- 5


    for (
      threshold in thresholds
    ) {

      settings <- rf_settings(
        storm_break_precip_mm =
          threshold
      )


      storms_split <- rf_identify_storms(
        x_split,
        settings = settings
      )


      storms_reset <- rf_identify_storms(
        x_reset,
        settings = settings
      )


      expect_equal(
        unique(
          storms_split$storm_id
        ),
        c(
          1L,
          2L
        ),
        info = paste(
          "storm_break_precip_mm =",
          threshold
        )
      )


      expect_equal(
        length(
          unique(
            storms_reset$storm_id
          )
        ),
        1,
        info = paste(
          "storm_break_precip_mm =",
          threshold
        )
      )


      expect_equal(
        sum(
          storms_split$precip_mm
        ),
        10,
        tolerance = 1e-12
      )


      expect_equal(
        sum(
          storms_reset$precip_mm
        ),
        10.001,
        tolerance = 1e-12
      )
    }
  }
)


test_that(
  "dry intervals inside a storm are retained",
  {

    x <- make_test_rain(181)

    # 00:00
    x$precip_mm[1] <- 5

    # 03:00
    x$precip_mm[181] <- 5

    storms <- rf_identify_storms(x)

    expect_equal(
      nrow(storms),
      181
    )

    expect_equal(
      sum(
        storms$precip_mm
      ),
      10
    )

    expect_equal(
      sum(
        storms$precip_mm == 0
      ),
      179
    )
  }
)


test_that(
  "sparse observations inside a storm are reconstructed on the regular grid",
  {

    x <- data.frame(
      datetime = as.POSIXct(
        c(
          "2025-01-01 01:35:00",
          "2025-01-01 02:52:00"
        ),
        tz = "UTC"
      ),
      precip_mm = c(
        0.03,
        0.01
      )
    )

    storms <- rf_identify_storms(
      x,
      interval_min = 1
    )

    # 01:35 through 02:52 inclusive = 78 one-minute positions.
    expect_equal(
      nrow(storms),
      78
    )

    expect_equal(
      unique(
        storms$storm_id
      ),
      1L
    )

    expect_equal(
      storms$datetime[1],
      as.POSIXct(
        "2025-01-01 01:35:00",
        tz = "UTC"
      )
    )

    expect_equal(
      storms$datetime[nrow(storms)],
      as.POSIXct(
        "2025-01-01 02:52:00",
        tz = "UTC"
      )
    )

    expect_equal(
      storms$precip_mm[1],
      0.03
    )

    expect_equal(
      storms$precip_mm[nrow(storms)],
      0.01
    )

    expect_equal(
      sum(
        storms$precip_mm
      ),
      0.04,
      tolerance = 1e-12
    )

    expect_equal(
      sum(
        storms$precip_mm == 0
      ),
      76
    )
  }
)


test_that(
  "explicit zero observations are preserved during sparse reconstruction",
  {

    x <- data.frame(
      datetime = as.POSIXct(
        c(
          "2025-01-01 00:00:00",
          "2025-01-01 00:05:00",
          "2025-01-01 00:10:00"
        ),
        tz = "UTC"
      ),
      precip_mm = c(
        1,
        0,
        2
      )
    )

    storms <- rf_identify_storms(
      x,
      interval_min = 1
    )

    expect_equal(
      nrow(storms),
      11
    )

    expect_equal(
      storms$precip_mm[
        storms$datetime ==
          as.POSIXct(
            "2025-01-01 00:05:00",
            tz = "UTC"
          )
      ],
      0
    )

    expect_equal(
      sum(
        storms$precip_mm
      ),
      3
    )
  }
)


test_that(
  "sparse rainfall more than six hours apart remains separate",
  {

    x <- data.frame(
      datetime = as.POSIXct(
        c(
          "2025-01-01 00:00:00",
          "2025-01-01 06:01:00"
        ),
        tz = "UTC"
      ),
      precip_mm = c(
        5,
        5
      )
    )

    storms <- rf_identify_storms(
      x,
      interval_min = 1
    )

    expect_equal(
      unique(
        storms$storm_id
      ),
      c(
        1L,
        2L
      )
    )

    # Each storm consists of only its single recorded rainfall minute.
    # We must not manufacture the six-hour dry period between
    # independent storms.
    expect_equal(
      nrow(storms),
      2
    )

    expect_equal(
      storms$precip_mm,
      c(
        5,
        5
      )
    )
  }
)


test_that(
  "an entirely dry series returns no storms",
  {

    x <- make_test_rain(500)

    storms <- rf_identify_storms(x)

    expect_s3_class(
      storms,
      "rf_storms"
    )

    expect_equal(
      nrow(storms),
      0
    )

    expect_equal(
      names(storms),
      c(
        "storm_id",
        "datetime",
        "precip_mm"
      )
    )
  }
)


test_that(
  "temporal resolution must be explicit when reader metadata are absent",
  {

    x <- data.frame(
      datetime = as.POSIXct(
        c(
          "2025-01-01 00:00:00",
          "2025-01-01 03:00:00"
        ),
        tz = "UTC"
      ),
      precip_mm = c(
        1,
        1
      )
    )

    expect_error(
      rf_identify_storms(x),
      "interval_min must be supplied"
    )
  }
)
