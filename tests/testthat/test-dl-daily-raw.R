test_that("smn_dl_daily_raw fails early on empty station", {
  expect_error(smn_dl_daily_raw(""))
})

test_that("smn_dl_daily_raw returns expected columns (smoke test)", {
  skip_on_cran()
  skip_if_offline()

  df <- smn_dl_daily_raw("15021")
  expect_true(all(c("date","prec","evap","tmax","tmin") %in% names(df)))
  expect_s3_class(df, "data.frame")
  expect_true(!is.null(attr(df, "source_url")))
})
