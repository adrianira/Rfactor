test_that(
  ".rf_less_than uses strict thresholds with numerical tolerance",
  {

    expect_false(
      Rfactor:::.rf_less_than(
        12.70,
        12.70
      )
    )


    expect_false(
      Rfactor:::.rf_less_than(
        12.70 - 1e-10,
        12.70
      )
    )


    expect_true(
      Rfactor:::.rf_less_than(
        12.69,
        12.70
      )
    )


    expect_false(
      Rfactor:::.rf_less_than(
        25.40,
        25.40
      )
    )


    expect_true(
      Rfactor:::.rf_less_than(
        25.39,
        25.40
      )
    )
  }
)
