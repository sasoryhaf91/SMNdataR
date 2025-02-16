#' Manejar errores con reintentos
#'
#' Evalúa una expresión y, en caso de error, reintenta su ejecución hasta alcanzar
#' el número máximo de intentos especificado.
#'
#' @param expr Expresión a evaluar.
#' @param max_attempts Número máximo de intentos (por defecto 1).
#' @return Resultado de la evaluación de la expresión. Si se agotan los intentos, se lanza un error.
#' @export
manejar_error <- function(expr, max_attempts = 3) {
  for (attempt in seq_len(max_attempts)) {
    result <- tryCatch(
      expr,
      error = function(e) {
        message(sprintf("Error en el intento %d: %s", attempt, conditionMessage(e)))
        NULL
      }
    )
    if (!is.null(result)) return(result)
  }
  stop("Se han agotado los intentos máximos.")
}
