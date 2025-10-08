# Silent + robust offline/HTTP reachability skip
skip_if_offline <- function(
    hosts = c("https://power.larc.nasa.gov",
              "https://smn.gob.mx",
              "https://www.google.com"),
    timeout_sec = 5
) {
  # 1) Sin internet (global)
  if (requireNamespace("curl", quietly = TRUE)) {
    if (!curl::has_internet())
      testthat::skip("Offline: no internet connection")
  }

  # 2) Probar varios hosts sin emitir warnings
  ok <- FALSE
  for (h in hosts) {
    domain <- sub("^https?://([^/]+).*", "\\1", h)

    # DNS check silencioso
    if (requireNamespace("curl", quietly = TRUE)) {
      res <- try(curl::nslookup(domain, error = FALSE), silent = TRUE)
      if (is.null(res)) next
    }

    # HEAD silencioso
    if (requireNamespace("httr2", quietly = TRUE)) {
      req <- httr2::request(h) |>
        httr2::req_method("HEAD") |>
        httr2::req_timeout(timeout_sec)
      ok <- isTRUE(try({ httr2::req_perform(req); TRUE }, silent = TRUE))
    } else if (requireNamespace("curl", quietly = TRUE)) {
      ok <- isTRUE(try({
        curl::curl_fetch_memory(h, handle = curl::new_handle(nobody = TRUE, timeout = timeout_sec))
        TRUE
      }, silent = TRUE))
    } else {
      # último recurso: base R sin warnings
      con <- suppressWarnings(try(url(h, "rb"), silent = TRUE))
      if (!inherits(con, "try-error")) { close(con); ok <- TRUE }
    }

    if (ok) break
  }

  if (!ok) {
    testthat::skip("Offline: hosts unreachable")
  }
  invisible(TRUE)
}
