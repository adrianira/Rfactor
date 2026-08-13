test_that(
  "rf_max_intensity calculates a rolling 30-minute maximum",
  {

    rain <- rep(
      0,
      90
    )

    # 30 mm falls from minute 21 through minute 50.
    #
    # This deliberately crosses conventional clock
    # half-hour boundaries.
    rain[21:50] <- 1

    result <- rf_max_intensity(
      precip_mm = rain,
      duration_min = 30,
      interval_min = 1
    )

    expect_equal(
      result,
      60
    )
  }
)


test_that(
  "rf_max_intensity calculates rolling I15",
  {

    rain <- rep(
      0,
      40
    )

    # 0.5 mm/min for 15 minutes
    # = 7.5 mm / 15 min
    # = 30 mm/h.
    rain[11:25] <- 0.5

    result <- rf_max_intensity(
      precip_mm = rain,
      duration_min = 15,
      interval_min = 1
    )

    expect_equal(
      result,
      30
    )
  }
)


test_that(
  "rf_max_intensity handles storms shorter than the requested duration",
  {

    rain <- rep(
      1,
      10
    )

    result <- rf_max_intensity(
      precip_mm = rain,
      duration_min = 30,
      interval_min = 1
    )

    expect_equal(
      result,
      20
    )
  }
)


test_that(
  "rf_max_intensity rejects incompatible intervals",
  {

    expect_error(
      rf_max_intensity(
        precip_mm = rep(
          0,
          100
        ),
        duration_min = 15,
        interval_min = 10
      ),
      "exact multiple"
    )
  }
)
