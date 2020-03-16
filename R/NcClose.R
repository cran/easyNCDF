#'Close a NEtCDF File
#'
#'@author N. Manubens \email{nicolau.manubens at bsc.es}
#'
#'@description Close a \code{ncdf4} open connection to a file.
#'
#'@param file_object NetCDF object as returned by \code{ncdf4::nc_open}.
#'
#'@return The result of \code{ncdf4::nc_close}.
#'
#'@import ncdf4
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
#'@export
NcClose <- function(file_object) {
  result <- NULL

  try({
    result <- nc_close(file_object)
  })

  invisible(result)
}
