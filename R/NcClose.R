NcClose <- function(file_object) {
  result <- NULL

  try({
    result <- nc_close(file_object)
  })

  invisible(result)
}
