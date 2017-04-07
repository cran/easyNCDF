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
      new_dim <- c(var = 1)
      found_dims <- c(new_dim, found_dims)
      if (!is.null(dims)) {
        dims <- .MergeArrayDims(dims, found_dims)
        dims <- pmax(dims[[1]], dims[[2]])
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

