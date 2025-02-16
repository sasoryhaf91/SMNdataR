testthat::test_that("descargar_datos_estacion retorna un data frame con las columnas esperadas y fechas en rango", {
  # Saltar la prueba si no hay conexión a Internet
  testthat::skip_if_offline("No hay conexión a Internet para descargar datos.")

  # Definir un rango de fechas de prueba
  fecha_inicio <- "2020-01-01"
  fecha_final  <- "2020-01-31"

  # Usar un código de estación conocido (por ejemplo, "15101")
  df <- descargar_datos_estacion("15101", fecha_inicio, fecha_final)

  # Verificar que el resultado es un data frame
  testthat::expect_s3_class(df, "data.frame")

  # Verificar que se tienen las columnas esperadas
  expected_cols <- c("Fecha", "Prec", "Evap", "Tmax", "Tmin")
  testthat::expect_true(all(expected_cols %in% names(df)))

  # Verificar que la columna 'Fecha' es de tipo Date
  testthat::expect_s3_class(df$Fecha, "Date")

  # Verificar que el rango de fechas esté dentro del especificado
  testthat::expect_true(min(df$Fecha, na.rm = TRUE) >= as.Date(fecha_inicio))
  testthat::expect_true(max(df$Fecha, na.rm = TRUE) <= as.Date(fecha_final))
})
