NcOpen <- function(file_path) {
  result <- NULL

  try({
    result <- nc_open(file_path)
  }, silent = TRUE)

  result
}
