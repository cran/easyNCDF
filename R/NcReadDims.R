#'Read Dimensions of a NetCDF File
#'
#'@author N. Manubens \email{nicolau.manubens at bsc.es}
#'@description Reads the dimension names and sizes of a set of variables in a NetCDF file, using the package \code{ncdf4}. The different variables in the file are considered to be stored along a dimension called 'var', so reading the dimensions of a variable 'foo' with dimensions 'lat' and 'lon' would result in a vector with the format c('var' = 1, 'lat' = n_lats, 'lon' = n_lons).
#'
#'@param file_to_read Path to the file to be read or a NetCDF object as returned by \code{easyNCDF::NcOpen} or \code{ncdf4::nc_open}.
#'@param var_names Vector of character strings with the names of the variables which to read the dimensions for. If multiple variables are requested, their dimensions will be merged and returned in a single vector.
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
#'
#'@export
NcReadDims <- function(file_to_read, var_names = NULL) {
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

  # Check var_names
  if (!is.null(var_names)) {
    if (!is.character(var_names)) {
      stop("Parameter 'var_names' must be a vector of character strings or NULL.")
    }
  }

  dims <- NULL
  if (!is.null(file_object)) {
    extra_dimvars <- NULL
    # Create all variables that are 'dimvars'
    for (dim_name in names(file_object$dim)) {
      if (file_object$dim[[dim_name]]$create_dimvar) {
        new_var <- list(name = dim_name, ndims = 1,
                        size = file_object$dim[[dim_name]]$len,
                        units = file_object$dim[[dim_name]]$units,
                        dim = list(file_object$dim[[dim_name]]))
        file_object$var[[dim_name]] <- new_var
        file_object$nvars <- file_object$ncars + 1
        extra_dimvars <- c(extra_dimvars, dim_name)
      }
    }
    if (is.null(var_names)) {
      var_names <- names(file_object$var)
    }
    for (var_name in var_names) {
      if (!(var_name %in% names(file_object$var))) {
        stop("Could not find the variable '", var_name, "' in the file.")
      }
      found_dims <- file_object$var[[var_name]]$size
      names(found_dims) <- sapply(file_object$var[[var_name]]$dim, '[[', 'name')
      # Support for character strings
      if ((file_object$var[[var_name]]$prec == 'char') && 
          (length(file_object$var[[var_name]][['dim']]) > 1)) {
        found_dims <- found_dims[-1]
      }
      new_dim <- c(var = 1)
      found_dims <- c(new_dim, found_dims)
      if (!is.null(dims)) {
        dims <- .MergeArrayDims(dims, found_dims)[[3]]
        dims['var'] <- dims['var'] + 1
      } else {
        dims <- found_dims
      }
    }
  }

  if (close) {
    file_closer(file_object)
  }

  dims
}

