test_that(
  "rf_settings returns the expected defaults",
  {

    x <- rf_settings()

    expect_s3_class(
      x,
      "rf_settings"
    )

    expect_equal(
      x$storm_break_hours,
      6
    )

    expect_equal(
      x$storm_break_precip_mm,
      1.27
    )

    expect_equal(
      x$omit_precip_below_mm,
      12.70
    )

    expect_equal(
      x$omit_intensity_below_mm_h,
      25.40
    )

    expect_equal(
      x$omit_intensity_duration_min,
      15
    )

    expect_equal(
      x$omit_logic,
      "all"
    )

    expect_equal(
      x$energy_equation,
      "brown_foster_1987"
    )

    expect_equal(
      x$intensity_durations_min,
      c(
        5,
        10,
        15,
        20,
        30,
        60
      )
    )

    expect_equal(
      x$single_record_energy,
      "calculate"
    )

    expect_true(
      x$omit_precip
    )

    expect_true(
      x$omit_intensity
    )
  }
)


test_that(
  "storm-break duration is customizable",
  {

    x <- rf_settings(
      storm_break_hours = 1.5
    )

    expect_equal(
      x$storm_break_hours,
      1.5
    )
  }
)


test_that(
  "storm-break precipitation setting accepts non-negative values",
  {

    values <- c(
      0,
      0.20,
      1.27,
      2,
      10
    )

    for (
      value in values
    ) {

      x <- rf_settings(
        storm_break_precip_mm = value
      )

      expect_equal(
        x$storm_break_precip_mm,
        value
      )
    }
  }
)


test_that(
  "all supported energy equations are accepted",
  {

    equations <- c(
      "brown_foster_1987",
      "mcgregor_1995",
      "laws_parsons_1943"
    )

    for (
      equation in equations
    ) {

      x <- rf_settings(
        energy_equation = equation
      )

      expect_equal(
        x$energy_equation,
        equation
      )
    }
  }
)


test_that(
  "all supported omission intensity durations are accepted",
  {

    durations <- c(
      5,
      10,
      15,
      30,
      60
    )

    for (
      duration in durations
    ) {

      x <- rf_settings(
        omit_intensity_duration_min =
          duration,

        intensity_durations_min =
          sort(
            unique(
              c(
                duration,
                30,
                60
              )
            )
          )
      )

      expect_equal(
        x$omit_intensity_duration_min,
        duration
      )
    }
  }
)


test_that(
  "omission logic can use all or any",
  {

    x_all <- rf_settings(
      omit_logic = "all"
    )

    x_any <- rf_settings(
      omit_logic = "any"
    )

    expect_equal(
      x_all$omit_logic,
      "all"
    )

    expect_equal(
      x_any$omit_logic,
      "any"
    )
  }
)


test_that(
  "precipitation-only omission is supported",
  {

    x <- rf_settings(
      omit_precip = TRUE,
      omit_intensity = FALSE
    )

    expect_true(
      x$omit_precip
    )

    expect_false(
      x$omit_intensity
    )
  }
)


test_that(
  "intensity-only omission is supported",
  {

    x <- rf_settings(
      omit_precip = FALSE,
      omit_intensity = TRUE
    )

    expect_false(
      x$omit_precip
    )

    expect_true(
      x$omit_intensity
    )
  }
)


test_that(
  "both omission criteria can be disabled",
  {

    x <- rf_settings(
      omit_precip = FALSE,
      omit_intensity = FALSE
    )

    expect_false(
      x$omit_precip
    )

    expect_false(
      x$omit_intensity
    )
  }
)


test_that(
  "omission thresholds are customizable",
  {

    x <- rf_settings(
      omit_precip_below_mm = 20,
      omit_intensity_below_mm_h = 30
    )

    expect_equal(
      x$omit_precip_below_mm,
      20
    )

    expect_equal(
      x$omit_intensity_below_mm_h,
      30
    )
  }
)


test_that(
  "reported intensity durations are customizable",
  {

    x <- rf_settings(
      intensity_durations_min = c(
        10,
        20,
        30,
        60
      ),

      omit_intensity_duration_min = 10
    )

    expect_equal(
      x$intensity_durations_min,
      c(
        10,
        20,
        30,
        60
      )
    )
  }
)


test_that(
  "disabled intensity omission does not require its duration to be reported",
  {

    x <- rf_settings(
      intensity_durations_min = c(
        30,
        60
      ),

      omit_intensity_duration_min = 15,

      omit_intensity = FALSE
    )

    expect_equal(
      x$intensity_durations_min,
      c(
        30,
        60
      )
    )

    expect_false(
      x$omit_intensity
    )
  }
)


test_that(
  "enabled intensity omission requires its duration to be reported",
  {

    expect_error(
      rf_settings(
        intensity_durations_min = c(
          30,
          60
        ),

        omit_intensity_duration_min = 15,

        omit_intensity = TRUE
      )
    )
  }
)


test_that(
  "single-record energy behavior is configurable",
  {

    x_calculate <- rf_settings(
      single_record_energy = "calculate"
    )

    x_rist <- rf_settings(
      single_record_energy = "rist_zero"
    )

    expect_equal(
      x_calculate$single_record_energy,
      "calculate"
    )

    expect_equal(
      x_rist$single_record_energy,
      "rist_zero"
    )
  }
)


test_that(
  "rf_settings rejects invalid storm-break values",
  {

    expect_error(
      rf_settings(
        storm_break_hours = 0
      )
    )

    expect_error(
      rf_settings(
        storm_break_hours = -1
      )
    )

    expect_error(
      rf_settings(
        storm_break_hours = Inf
      )
    )

    expect_error(
      rf_settings(
        storm_break_precip_mm = -0.01
      )
    )

    expect_error(
      rf_settings(
        storm_break_precip_mm = Inf
      )
    )
  }
)


test_that(
  "rf_settings rejects unknown energy equations",
  {

    expect_error(
      rf_settings(
        energy_equation = "unknown"
      )
    )
  }
)


test_that(
  "rf_settings rejects unknown omission logic",
  {

    expect_error(
      rf_settings(
        omit_logic = "unknown"
      )
    )
  }
)


test_that(
  "rf_settings rejects unsupported omission intensity durations",
  {

    expect_error(
      rf_settings(
        omit_intensity_duration_min = 20
      )
    )

    expect_error(
      rf_settings(
        omit_intensity_duration_min = 7
      )
    )
  }
)


test_that(
  "rf_settings rejects invalid omission switches",
  {

    expect_error(
      rf_settings(
        omit_precip = NA
      )
    )

    expect_error(
      rf_settings(
        omit_intensity = NA
      )
    )

    expect_error(
      rf_settings(
        omit_precip = 1
      )
    )

    expect_error(
      rf_settings(
        omit_intensity = "yes"
      )
    )
  }
)


test_that(
  "rf_settings rejects unknown single-record energy behavior",
  {

    expect_error(
      rf_settings(
        single_record_energy = "unknown"
      )
    )
  }
)
