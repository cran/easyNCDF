#'Read Names of Variables in a NetCDF File
#'
#'@author N. Manubens \email{nicolau.manubens@bsc.es} 
#'
#'@description Reads the names of the variables in a NetCDF file and returns them as a vector of character strings.
#'
#'@param file_to_read Path to the file to be read or a NetCDF object as returned by \code{easyNCDF::NcOpen} or \code{ncdf4::nc_open}.
#'
#'@return Vector of character strings with the names of the variables in the NetCDF file.
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
NcReadVarNames <- function(file_to_read) {
  file_opener <- nc_open
  file_closer <- nc_close
  close <- FALSE
  if (is.character(file_to_read)) {
    file_object <- file_opener(file_to_read)
    file_path <- file_to_read
    close <- TRUE
  } else if (grepl('^ncdf', class(file_to_read))) {
    file_object <- file_to_read
    file_path <- file_object$filename
  } else {
    stop("Either the path to a NetCDF file or a ncdf object must be provided as 'file_to_read'.")
  }

  var_names <- names(file_object$var)
  if (!is.null(file_object)) {
    extra_dimvars <- NULL
    # Create all variables that are 'dimvars'
    for (dim_name in names(file_object$dim)) {
      if (file_object$dim[[dim_name]]$create_dimvar) {
        extra_dimvars <- c(extra_dimvars, dim_name)
      }
    }
    var_names <- c(var_names, extra_dimvars)
  }

  if (close) {
    file_closer(file_object)
  }

  var_names
}
