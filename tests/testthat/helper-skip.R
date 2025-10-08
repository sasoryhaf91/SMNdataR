skip_if_offline <- function(host = "https://smn.gob.mx") {
  ok <- tryCatch({ con <- url(host, "rb"); close(con); TRUE }, error = function(e) FALSE)
  if (!ok) testthat::skip(paste("Offline:", host, "unreachable"))
}
