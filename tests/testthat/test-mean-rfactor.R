test_that(
  "yearly mean omits missing values and retains genuine zeros",
  {

    x <- data.frame(
      year = 2020:2024,
      R = c(
        800,
        900,
        NA,
        700,
        0
      )
    )

    result <- rf_calculate_mean_rfactor(
      x,
      period = "yearly"
    )

    expect_s3_class(
      result,
      "data.frame"
    )

    expect_equal(
      names(
        result
      ),
      c(
        "mean_R",
        "n_years"
      )
    )

    expect_equal(
      result$mean_R,
      600
    )

    expect_equal(
      result$n_years,
      4
    )
  }
)


test_that(
  "yearly calculation returns NA when all R values are missing",
  {

    x <- data.frame(
      year = 2020:2022,
      R = c(
        NA_real_,
        NA_real_,
        NA_real_
      )
    )

    result <- rf_calculate_mean_rfactor(
      x,
      period = "yearly"
    )

    expect_true(
      is.na(
        result$mean_R
      )
    )

    expect_equal(
      result$n_years,
      0
    )
  }
)


test_that(
  "yearly is the default temporal scale",
  {

    x <- data.frame(
      year = 2020:2022,
      R = c(
        100,
        200,
        300
      )
    )

    default_result <- rf_calculate_mean_rfactor(
      x
    )

    yearly_result <- rf_calculate_mean_rfactor(
      x,
      period = "yearly"
    )

    expect_equal(
      default_result,
      yearly_result
    )
  }
)


test_that(
  "monthly calculation produces a separate mean for each available month",
  {

    x <- data.frame(
      year = c(
        2022,
        2020,
        2021,
        2022,
        2020,
        2021,
        2020,
        2021
      ),
      month = c(
        1,
        1,
        1,
        2,
        2,
        2,
        4,
        4
      ),
      R = c(
        NA,
        10,
        20,
        80,
        0,
        40,
        NA,
        NA
      )
    )

    result <- rf_calculate_mean_rfactor(
      x,
      period = "monthly"
    )

    expect_equal(
      names(
        result
      ),
      c(
        "month",
        "mean_R",
        "n_years"
      )
    )

    expect_equal(
      result$month,
      c(
        1,
        2,
        4
      )
    )

    expect_equal(
      result$mean_R[
        result$month == 1
      ],
      15
    )

    expect_equal(
      result$n_years[
        result$month == 1
      ],
      2
    )

    expect_equal(
      result$mean_R[
        result$month == 2
      ],
      40
    )

    expect_equal(
      result$n_years[
        result$month == 2
      ],
      3
    )

    expect_true(
      is.na(
        result$mean_R[
          result$month == 4
        ]
      )
    )

    expect_equal(
      result$n_years[
        result$month == 4
      ],
      0
    )
  }
)


test_that(
  "completely absent calendar months are not generated",
  {

    x <- data.frame(
      year = c(
        2020,
        2021,
        2020,
        2021
      ),
      month = c(
        1,
        1,
        3,
        3
      ),
      R = c(
        10,
        20,
        30,
        40
      )
    )

    result <- rf_calculate_mean_rfactor(
      x,
      period = "monthly"
    )

    expect_equal(
      result$month,
      c(
        1,
        3
      )
    )

    expect_false(
      2 %in%
        result$month
    )
  }
)


test_that(
  "additional R-factor columns do not affect the calculation",
  {

    x <- data.frame(
      year = 2020:2022,
      R = c(
        100,
        200,
        300
      ),
      n_events = c(
        3,
        5,
        4
      )
    )

    result <- rf_calculate_mean_rfactor(
      x,
      period = "yearly"
    )

    expect_equal(
      result$mean_R,
      200
    )

    expect_equal(
      result$n_years,
      3
    )
  }
)


test_that(
  "input must be a non-empty data frame",
  {

    expect_error(
      rf_calculate_mean_rfactor(
        list(
          year = 2020,
          R = 100
        )
      ),
      "rfactor must be a data frame"
    )

    expect_error(
      rf_calculate_mean_rfactor(
        data.frame(
          year = numeric(),
          R = numeric()
        )
      ),
      "at least one row"
    )
  }
)


test_that(
  "required columns are checked",
  {

    expect_error(
      rf_calculate_mean_rfactor(
        data.frame(
          year = 2020:2021
        ),
        period = "yearly"
      ),
      "missing required column"
    )

    expect_error(
      rf_calculate_mean_rfactor(
        data.frame(
          year = 2020:2021,
          R = c(
            100,
            200
          )
        ),
        period = "monthly"
      ),
      "missing required column"
    )
  }
)


test_that(
  "year must be numeric, finite, and integer-valued",
  {

    expect_error(
      rf_calculate_mean_rfactor(
        data.frame(
          year = c(
            "2020",
            "2021"
          ),
          R = c(
            100,
            200
          )
        )
      ),
      "year must be numeric"
    )

    expect_error(
      rf_calculate_mean_rfactor(
        data.frame(
          year = c(
            2020,
            NA
          ),
          R = c(
            100,
            200
          )
        )
      ),
      "year must not contain missing"
    )

    expect_error(
      rf_calculate_mean_rfactor(
        data.frame(
          year = c(
            2020,
            Inf
          ),
          R = c(
            100,
            200
          )
        )
      ),
      "year must not contain missing or non-finite"
    )

    expect_error(
      rf_calculate_mean_rfactor(
        data.frame(
          year = c(
            2020,
            2021.5
          ),
          R = c(
            100,
            200
          )
        )
      ),
      "integer-valued years"
    )
  }
)


test_that(
  "R values must be numeric, finite when available, and non-negative",
  {

    expect_error(
      rf_calculate_mean_rfactor(
        data.frame(
          year = 2020:2021,
          R = c(
            "100",
            "200"
          )
        )
      ),
      "R must be numeric"
    )

    expect_error(
      rf_calculate_mean_rfactor(
        data.frame(
          year = 2020:2021,
          R = c(
            100,
            Inf
          )
        )
      ),
      "R must not contain infinite values"
    )

    expect_error(
      rf_calculate_mean_rfactor(
        data.frame(
          year = 2020:2021,
          R = c(
            100,
            -1
          )
        )
      ),
      "R must not contain negative values"
    )
  }
)


test_that(
  "yearly input cannot contain duplicated years",
  {

    x <- data.frame(
      year = c(
        2020,
        2020
      ),
      R = c(
        100,
        200
      )
    )

    expect_error(
      rf_calculate_mean_rfactor(
        x,
        period = "yearly"
      ),
      "at most one R-factor value for each year"
    )
  }
)


test_that(
  "month must be numeric, finite, integer-valued, and between 1 and 12",
  {

    expect_error(
      rf_calculate_mean_rfactor(
        data.frame(
          year = 2020:2021,
          month = c(
            "1",
            "2"
          ),
          R = c(
            100,
            200
          )
        ),
        period = "monthly"
      ),
      "month must be numeric"
    )

    expect_error(
      rf_calculate_mean_rfactor(
        data.frame(
          year = 2020:2021,
          month = c(
            1,
            NA
          ),
          R = c(
            100,
            200
          )
        ),
        period = "monthly"
      ),
      "month must not contain missing"
    )

    expect_error(
      rf_calculate_mean_rfactor(
        data.frame(
          year = 2020:2021,
          month = c(
            1,
            Inf
          ),
          R = c(
            100,
            200
          )
        ),
        period = "monthly"
      ),
      "month must not contain missing or non-finite"
    )

    expect_error(
      rf_calculate_mean_rfactor(
        data.frame(
          year = 2020:2021,
          month = c(
            1,
            2.5
          ),
          R = c(
            100,
            200
          )
        ),
        period = "monthly"
      ),
      "integer-valued calendar months"
    )

    expect_error(
      rf_calculate_mean_rfactor(
        data.frame(
          year = 2020:2021,
          month = c(
            0,
            13
          ),
          R = c(
            100,
            200
          )
        ),
        period = "monthly"
      ),
      "calendar months from 1 to 12"
    )
  }
)


test_that(
  "monthly input cannot contain duplicated year-month combinations",
  {

    x <- data.frame(
      year = c(
        2020,
        2020
      ),
      month = c(
        5,
        5
      ),
      R = c(
        100,
        200
      )
    )

    expect_error(
      rf_calculate_mean_rfactor(
        x,
        period = "monthly"
      ),
      "at most one R-factor value for each year-month combination"
    )
  }
)
