test_that(
  "rf_calculate_energy reproduces Brown-Foster unit energy",
  {

    # 1 mm falls during one 1-minute interval.
    #
    # Intensity = 60 mm/h.
    #
    # Brown-Foster:
    # e = 0.29 * (1 - 0.72 * exp(-0.05 * 60))
    #
    # Expected energy:
    # 0.27960446012479 MJ/ha

    result <- rf_calculate_energy(
      precip_mm = 1,
      interval_min = 1,
      energy_equation = "brown_foster_1987"
    )

    expect_equal(
      result,
      0.27960446012479,
      tolerance = 1e-12
    )
  }
)


test_that(
  "Brown-Foster remains the default energy equation",
  {

    default_result <- rf_calculate_energy(
      precip_mm = 1,
      interval_min = 1
    )

    explicit_result <- rf_calculate_energy(
      precip_mm = 1,
      interval_min = 1,
      energy_equation = "brown_foster_1987"
    )

    expect_equal(
      default_result,
      explicit_result,
      tolerance = 1e-12
    )
  }
)


test_that(
  "rf_calculate_energy reproduces McGregor 1995 unit energy",
  {

    # 1 mm falls during one 1-minute interval.
    #
    # Intensity = 60 mm/h.
    #
    # McGregor et al.:
    # e = 0.29 * (1 - 0.72 * exp(-0.082 * 60))
    #
    # Expected energy:
    # 0.2884759414791905 MJ/ha

    result <- rf_calculate_energy(
      precip_mm = 1,
      interval_min = 1,
      energy_equation = "mcgregor_1995"
    )

    expect_equal(
      result,
      0.2884759414791905,
      tolerance = 1e-12
    )
  }
)


test_that(
  "rf_calculate_energy reproduces Laws-Parsons unit energy",
  {

    # 1 mm falls during one 1-minute interval.
    #
    # Intensity = 60 mm/h.
    #
    # Laws-Parsons:
    # e = 0.119 + 0.0873 * log10(60)
    #
    # Expected energy:
    # 0.2742326041584921 MJ/ha

    result <- rf_calculate_energy(
      precip_mm = 1,
      interval_min = 1,
      energy_equation = "laws_parsons_1943"
    )

    expect_equal(
      result,
      0.2742326041584921,
      tolerance = 1e-12
    )
  }
)


test_that(
  "Laws-Parsons is applied above 76.2 mm per hour",
  {

    # 2 mm during one 1-minute interval corresponds to:
    #
    # 120 mm/h
    #
    # Dedicated validation against RIST 3.99.10 showed that
    # Laws-Parsons is applied directly at 120 mm/h without
    # an upper intensity cap.
    #
    # e = 0.119 + 0.0873 * log10(120)
    #
    # Since rainfall depth is 2 mm:
    #
    # E = 2 * e

    result <- rf_calculate_energy(
      precip_mm = 2,
      interval_min = 1,
      energy_equation = "laws_parsons_1943"
    )

    expect_equal(
      result,
      0.6010250455599153,
      tolerance = 1e-12
    )
  }
)


test_that(
  "zero rainfall contributes zero event energy for all equations",
  {

    equations <- c(
      "brown_foster_1987",
      "mcgregor_1995",
      "laws_parsons_1943"
    )

    for (equation in equations) {

      result <- rf_calculate_energy(
        precip_mm = rep(
          0,
          60
        ),
        interval_min = 1,
        energy_equation = equation
      )

      expect_equal(
        result,
        0
      )
    }
  }
)


test_that(
  "rf_calculate_energy sums energy over intervals",
  {

    # 30 minutes with 1 mm/min.
    #
    # Each minute:
    # intensity = 60 mm/h
    #
    # Brown-Foster total expected energy:
    # 30 * 0.27960446012479

    result <- rf_calculate_energy(
      precip_mm = rep(
        1,
        30
      ),
      interval_min = 1,
      energy_equation = "brown_foster_1987"
    )

    expect_equal(
      result,
      8.3881338037437,
      tolerance = 1e-12
    )
  }
)


test_that(
  "different energy equations produce different results",
  {

    precip <- rep(
      1,
      10
    )

    brown_foster <- rf_calculate_energy(
      precip_mm = precip,
      interval_min = 1,
      energy_equation = "brown_foster_1987"
    )

    mcgregor <- rf_calculate_energy(
      precip_mm = precip,
      interval_min = 1,
      energy_equation = "mcgregor_1995"
    )

    laws_parsons <- rf_calculate_energy(
      precip_mm = precip,
      interval_min = 1,
      energy_equation = "laws_parsons_1943"
    )

    expect_false(
      isTRUE(
        all.equal(
          brown_foster,
          mcgregor
        )
      )
    )

    expect_false(
      isTRUE(
        all.equal(
          brown_foster,
          laws_parsons
        )
      )
    )

    expect_false(
      isTRUE(
        all.equal(
          mcgregor,
          laws_parsons
        )
      )
    )
  }
)


test_that(
  "rf_calculate_energy rejects unknown energy equations",
  {

    expect_error(
      rf_calculate_energy(
        precip_mm = 1,
        interval_min = 1,
        energy_equation = "unknown_equation"
      ),
      "arg"
    )
  }
)


test_that(
  "rf_calculate_energy rejects negative rainfall",
  {

    expect_error(
      rf_calculate_energy(
        c(
          0,
          1,
          -0.1
        )
      ),
      "negative"
    )
  }
)
