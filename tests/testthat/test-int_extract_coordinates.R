test_that("returns NA row if page is empty or labels missing", {
  # Difícil de simular sin red; aquí solo estructura:
  out <- data.frame(latitude=NA_real_, longitude=NA_real_, altitude=NA_real_)
  expect_true(all(names(smn_int_extract_coordinates("00000")) == names(out)))
})

test_that("adds meta columns when requested (shape only)", {
  # usando un id real si hay red; si no, solo tipo:
  res <- smn_int_extract_coordinates("15021", add_meta_cols = TRUE)
  expect_true(all(c("latitude","longitude","altitude","station","source_url") %in% names(res)))
})
