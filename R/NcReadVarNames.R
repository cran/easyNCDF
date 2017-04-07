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

  var_names
}
