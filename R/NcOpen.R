#'Open a NetCDF File
#'
#'@author N. Manubens \email{nicolau.manubens@bsc.es}
#'
#'@description Silently opens a NetCDF file with \code{ncdf4::nc_open}. Returns NULL on failure.
#'
#'@param file_path Character string with the path to the file to be opened.
#'
#'@import ncdf4
#'
#'@return A NetCDF object as returned by \code{ncdf4::nc_open} or NULL on failure.
#'@examples
#'# Create an array from R
#'file_path <- tempfile(fileext = '.nc')
#'a <- array(1:9, dim = c(member = 3, time = 3))
#'# Store into a NetCDF twice, as two different variables
#'ArrayToNc(list(var_1 = a, var_2 = a + 1), file_path)
#'# Read the dimensions and variables in the created file
#'fnc <- NcOpen(file_path)
#'fnc_dims <- NcReadDims(fnc)
#'var_names <- NcReadVarNames(fnc)
#'# Read the two variables from the file into an R array
#'a_from_file <- NcToArray(fnc, vars_to_read = var_names)
#'NcClose(fnc)
#'# Check the obtained array matches the original array
#'print(a)
#'print(a_from_file[1, , ])
#'
#'@export
NcOpen <- function(file_path) {
  result <- NULL

  try({
    result <- nc_open(file_path)
  }, silent = TRUE)

  result
}
