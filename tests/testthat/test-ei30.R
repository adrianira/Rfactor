# -------------------------------------------------------------------------
# Helper: create a synthetic 1-minute storm for EI30 tests
# -------------------------------------------------------------------------

make_rf_test_storm <- function(
    precip_mm,
    start = "2025-07-01 12:00:00",
    settings = rf_settings()
) {

  n <- length(
    precip_mm
  )

  x <- data.frame(
    storm_id =
      rep(
        1L,
        n
      ),

    datetime =
      as.POSIXct(
        start,
        tz = "UTC"
      ) +
      seq(
        from = 0,
        by = 60,
        length.out = n
      ),

    precip_mm =
      precip_mm
  )

  class(x) <- c(
    "rf_storms",
    "data.frame"
  )

  attr(
    x,
    "settings"
  ) <- settings

  attr(
    x,
    "interval_min"
  ) <- 1

  x
}


# -------------------------------------------------------------------------
# Tests
# -------------------------------------------------------------------------

test_that(
  "rf_calculate_ei30 calculates a known 30-minute storm",
  {

    # 1 mm/min for exactly 30 minutes.
    #
    # Total rainfall = 30 mm
    #
    # Intensity during every wet minute = 60 mm/h
    #
    # I30 = 60 mm/h
    #
    # Brown-Foster event energy =
    # 8.3881338037437 MJ/ha
    #
    # EI30 =
    # 8.3881338037437 * 60
    # =
    # 503.288028224622

    storm <- make_rf_test_storm(
      rep(
        1,
        30
      )
    )

    result <-
      rf_calculate_ei30(
        storm
      )

    expect_equal(
      result$precip_mm,
      30
    )

    expect_equal(
      result$i30_mm_h,
      60
    )

    expect_equal(
      result$energy_mj_ha,
      8.3881338037437,
      tolerance = 1e-10
    )

    expect_equal(
      result$ei30,
      503.288028224622,
      tolerance = 1e-9
    )

    expect_true(
      result$erosive
    )

    expect_false(
      result$omitted
    )
  }
)


test_that(
  "small low-intensity storm is omitted",
  {

    # 5 mm over 30 minutes.
    #
    # P < 12.7
    # I15 < 25.4
    #
    # BOTH omission conditions TRUE
    # -> omitted

    storm <-
      make_rf_test_storm(
        rep(
          5 / 30,
          30
        )
      )

    result <-
      rf_calculate_ei30(
        storm
      )

    expect_true(
      result$precip_mm < 12.7
    )

    expect_true(
      result$i15_mm_h < 25.4
    )

    expect_true(
      result$omitted
    )

    expect_false(
      result$erosive
    )
  }
)


test_that(
  "small but intense storm is erosive",
  {

    # 7 mm falls in 7 minutes.
    #
    # Total precipitation:
    # 7 mm < 12.7
    #
    # Maximum 15-min intensity:
    # 7 / 15 * 60
    # = 28 mm/h
    #
    # Therefore it must NOT be omitted.

    rain <- c(
      rep(
        1,
        7
      ),
      rep(
        0,
        23
      )
    )

    storm <-
      make_rf_test_storm(
        rain
      )

    result <-
      rf_calculate_ei30(
        storm
      )

    expect_equal(
      result$precip_mm,
      7
    )

    expect_equal(
      result$i15_mm_h,
      28
    )

    expect_true(
      result$omit_precip_condition
    )

    expect_false(
      result$omit_intensity_condition
    )

    expect_false(
      result$omitted
    )

    expect_true(
      result$erosive
    )
  }
)


test_that(
  "large low-intensity storm is erosive",
  {

    # 13 mm uniformly over 120 minutes.
    #
    # P >= 12.7
    #
    # Therefore the storm stays even if I15 is low.

    storm <-
      make_rf_test_storm(
        rep(
          13 / 120,
          120
        )
      )

    result <-
      rf_calculate_ei30(
        storm
      )

    expect_equal(
      result$precip_mm,
      13,
      tolerance = 1e-10
    )

    expect_true(
      result$i15_mm_h < 25.4
    )

    expect_false(
      result$omit_precip_condition
    )

    expect_true(
      result$omit_intensity_condition
    )

    expect_false(
      result$omitted
    )

    expect_true(
      result$erosive
    )
  }
)


test_that(
  "exactly 12.7 mm passes the precipitation threshold",
  {

    storm <-
      make_rf_test_storm(
        rep(
          12.7 / 120,
          120
        )
      )

    result <-
      rf_calculate_ei30(
        storm
      )

    expect_equal(
      result$precip_mm,
      12.7,
      tolerance = 1e-10
    )

    expect_false(
      result$omit_precip_condition
    )

    expect_true(
      result$erosive
    )
  }
)


test_that(
  "exactly 25.4 mm/h passes the intensity threshold",
  {

    # 6.35 mm in exactly 15 minutes:
    #
    # 6.35 / 15 * 60
    # = 25.4 mm/h

    storm <-
      make_rf_test_storm(
        rep(
          6.35 / 15,
          15
        )
      )

    result <-
      rf_calculate_ei30(
        storm
      )

    expect_equal(
      result$i15_mm_h,
      25.4,
      tolerance = 1e-10
    )

    expect_false(
      result$omit_intensity_condition
    )

    expect_true(
      result$erosive
    )
  }
)


test_that(
  "one-record storm energy is calculated by default",
  {

    # 13 mm in one 1-minute rainfall record.
    #
    # I30 = 13 / 30 * 60 = 26 mm/h
    #
    # Rfactor's default behaviour is to calculate
    # kinetic energy normally.

    settings <- rf_settings(
      single_record_energy =
        "calculate"
    )

    storm <- make_rf_test_storm(
      precip_mm = 13,
      settings = settings
    )

    result <- rf_calculate_ei30(
      storm,
      settings = settings
    )

    expect_equal(
      result$precip_mm,
      13
    )

    expect_equal(
      result$i30_mm_h,
      26
    )

    expect_equal(
      result$energy_mj_ha,
      3.77,
      tolerance = 1e-8
    )

    expect_equal(
      result$ei30,
      98.02,
      tolerance = 1e-8
    )

    expect_false(
      result$omitted
    )

    expect_true(
      result$erosive
    )
  }
)


test_that(
  "RIST compatibility assigns zero energy to a one-record storm",
  {

    # 7 mm in one 1-minute rainfall record.
    #
    # P = 7 mm < 12.7 mm
    #
    # I15 = 28 mm/h > 25.4 mm/h
    #
    # Therefore the event is still classified as erosive,
    # but RIST 3.99.10 reports zero energy and zero EI30
    # for the one-record storm.

    settings <- rf_settings(
      single_record_energy =
        "rist_zero"
    )

    storm <- make_rf_test_storm(
      precip_mm = 7,
      settings = settings
    )

    result <- rf_calculate_ei30(
      storm,
      settings = settings
    )

    expect_equal(
      result$precip_mm,
      7
    )

    expect_equal(
      result$i15_mm_h,
      28
    )

    expect_equal(
      result$i30_mm_h,
      14
    )

    expect_equal(
      result$energy_mj_ha,
      0
    )

    expect_equal(
      result$ei30,
      0
    )

    expect_true(
      result$omit_precip_condition
    )

    expect_false(
      result$omit_intensity_condition
    )

    expect_false(
      result$omitted
    )

    expect_true(
      result$erosive
    )
  }
)


test_that(
  "RIST zero-energy behaviour applies only to one-record storms",
  {

    # The same total 13 mm represented by two consecutive
    # rainfall records must be calculated normally.

    settings <- rf_settings(
      single_record_energy =
        "rist_zero"
    )

    storm <- make_rf_test_storm(
      precip_mm = c(
        6.5,
        6.5
      ),
      settings = settings
    )

    result <- rf_calculate_ei30(
      storm,
      settings = settings
    )

    expect_equal(
      result$precip_mm,
      13
    )

    expect_equal(
      result$i30_mm_h,
      26
    )

    expect_equal(
      result$energy_mj_ha,
      3.77,
      tolerance = 1e-8
    )

    expect_equal(
      result$ei30,
      98.02,
      tolerance = 1e-8
    )

    expect_false(
      result$omitted
    )

    expect_true(
      result$erosive
    )
  }
)


test_that(
  "both omission criteria disabled includes all storms",
  {

    # This storm would normally be omitted because:
    #
    # P < 12.7 mm
    # AND
    # I15 < 25.4 mm/h
    #
    # With both omission criteria disabled, it must be retained.

    settings <- rf_settings(
      omit_precip = FALSE,
      omit_intensity = FALSE
    )

    storm <- make_rf_test_storm(
      precip_mm = rep(
        5 / 30,
        30
      ),
      settings = settings
    )

    result <- rf_calculate_ei30(
      storm,
      settings = settings
    )

    expect_true(
      is.na(
        result$omit_precip_condition
      )
    )

    expect_true(
      is.na(
        result$omit_intensity_condition
      )
    )

    expect_false(
      result$omitted
    )

    expect_true(
      result$erosive
    )
  }
)


test_that(
  "ANY omission logic omits when only precipitation condition is met",
  {

    # 7 mm in 7 minutes:
    #
    # P < 12.7       -> TRUE
    # I15 = 28       -> FALSE
    #
    # ALL -> retained
    # ANY -> omitted

    settings <- rf_settings(
      omit_logic = "any"
    )

    rain <- c(
      rep(
        1,
        7
      ),
      rep(
        0,
        23
      )
    )

    storm <- make_rf_test_storm(
      precip_mm = rain,
      settings = settings
    )

    result <- rf_calculate_ei30(
      storm,
      settings = settings
    )

    expect_true(
      result$omit_precip_condition
    )

    expect_false(
      result$omit_intensity_condition
    )

    expect_true(
      result$omitted
    )

    expect_false(
      result$erosive
    )
  }
)


test_that(
  "precipitation-only omission uses only precipitation criterion",
  {

    # Small but intense storm.
    #
    # Normally:
    #
    # P < 12.7       -> TRUE
    # I15 > 25.4     -> FALSE
    #
    # With precipitation as the only selected criterion,
    # the storm must be omitted.

    settings <- rf_settings(
      omit_precip = TRUE,
      omit_intensity = FALSE
    )

    rain <- c(
      rep(
        1,
        7
      ),
      rep(
        0,
        23
      )
    )

    storm <- make_rf_test_storm(
      precip_mm = rain,
      settings = settings
    )

    result <- rf_calculate_ei30(
      storm,
      settings = settings
    )

    expect_true(
      result$omit_precip_condition
    )

    expect_true(
      is.na(
        result$omit_intensity_condition
      )
    )

    expect_true(
      result$omitted
    )

    expect_false(
      result$erosive
    )
  }
)


test_that(
  "intensity-only omission uses only intensity criterion",
  {

    # 13 mm spread uniformly over 120 minutes.
    #
    # P >= 12.7
    #
    # but I15 is low.
    #
    # With intensity as the only criterion, the storm
    # must therefore be omitted.

    settings <- rf_settings(
      omit_precip = FALSE,
      omit_intensity = TRUE
    )

    storm <- make_rf_test_storm(
      precip_mm = rep(
        13 / 120,
        120
      ),
      settings = settings
    )

    result <- rf_calculate_ei30(
      storm,
      settings = settings
    )

    expect_true(
      is.na(
        result$omit_precip_condition
      )
    )

    expect_true(
      result$omit_intensity_condition
    )

    expect_true(
      result$omitted
    )

    expect_false(
      result$erosive
    )
  }
)


test_that(
  "non-default omission intensity duration is used",
  {

    # 3 mm during the first 5 minutes.
    #
    # I5:
    # 3 / 5 * 60 = 36 mm/h
    #
    # I15:
    # 3 / 15 * 60 = 12 mm/h
    #
    # With the omission criterion based on I5 and threshold
    # 25.4 mm/h, the intensity condition is FALSE and the
    # storm remains included.

    settings <- rf_settings(
      omit_precip = FALSE,
      omit_intensity = TRUE,
      omit_intensity_duration_min = 5
    )

    rain <- c(
      rep(
        3 / 5,
        5
      ),
      rep(
        0,
        25
      )
    )

    storm <- make_rf_test_storm(
      precip_mm = rain,
      settings = settings
    )

    result <- rf_calculate_ei30(
      storm,
      settings = settings
    )

    expect_equal(
      result$i5_mm_h,
      36,
      tolerance = 1e-10
    )

    expect_equal(
      result$i15_mm_h,
      12,
      tolerance = 1e-10
    )

    expect_false(
      result$omit_intensity_condition
    )

    expect_false(
      result$omitted
    )

    expect_true(
      result$erosive
    )
  }
)


test_that(
  "EI30 requires a temporal resolution compatible with 30 minutes",
  {

    storms <- data.frame(
      storm_id = 1L,
      datetime = as.POSIXct(
        c(
          "2025-01-01 00:00:00",
          "2025-01-01 00:20:00"
        ),
        tz = "UTC"
      ),
      precip_mm = c(
        1,
        1
      )
    )

    expect_error(
      rf_calculate_ei30(
        storms,
        settings = rf_settings(
          intensity_durations_min = c(
            20,
            30,
            60
          ),
          omit_intensity = FALSE
        ),
        interval_min = 20
      ),
      "must divide 30 minutes exactly"
    )
  }
)
