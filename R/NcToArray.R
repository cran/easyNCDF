#'Read Names of Variables in a NetCDF File
#'
#'@author N. Manubens, \email{nicolau.manubens@bsc.es}
#'
#'@description Reads the names of the variables in a NetCDF file and returns them as a vector of character strings.
#'
#'@param file_to_read Path to the file to be read or a NetCDF object as returned by \code{easyNCDF::NcOpen} or \code{ncdf4::nc_open}.
#'@param dim_indices Named list with numeric vectors of indices to take for each dimension. The names should correspond to the dimension names which to take the indices for. Non-consecutive indices can be specified. If \code{expect_all_indices = FALSE} (default), it is not mandatory to specify the indices for all (or even any of) the dimensions. In that case all the indices along such dimensions will be read in. If \code{expect_all_indices = TRUE}, then indices for all the dimensions have to be specified for the function to return a data array. In that case, \code{NA} can be used to request all indices for a dimension if desired.
#'\cr\cr
#'Since this function considers the variables in a NetCDF file are stored along a 'var' dimension, indices for the (actually non-existing) 'var'/'variable' dimension can be specified. They can be specified in 3 ways:\cr
#' - A vector of numeric indices: e.g. \code{list(var = c(1, 3, 5))} to take the 1st, 3rd and 5th found variables.\cr
#' - A vector of character strings with variable names: e.g. \code{list(var = c('foo', 'bar'))}.\cr
#' - A list of vectors with numeric indices or character strings: e.g. \code{list(var = list(c(1, 3, 'foo'), c(2, 'bar')))}\cr
#'Vectors with combined numeric indices and character strings are accepted.\cr
#'Whereas the first two options will return a single extended array with the merged variables, the second option will return a list with an array for each requested variable.
#'
#'@param vars_to_read This parameter is a shortcut to (and has less priority than) specifying the requested variable names via \code{dim_indices = list(var = ...)}. It is useful when all the indices for all the requested variables have to be taken, so the parameter \code{dim_indices} can be skipped, but still only a specific variable or set of variables have to be taken. Check the documentation for the parameter \code{dim_indices} to see the three possible ways to specify this parameter.
#'
#'@param drop_var_dim Whether to drop the 'var' dimension this function assumes (read description). If multiple variables are requested in a vector and \code{unlist = TRUE}, the drop won't be performed (not possible).
#'
#'@param unlist Whether to merge the resulting array variables into a single array if possible (default) or not. Otherwise a list with as many arrays as requested variables is returned.
#'
#'@param expect_all_indices Whether the function should stop if indices are not provided for all the dimensions of any of the requested variables (TRUE) rather than assuming that all the indices are requested for the unspecified dimensions (FALSE). By default the later is done (FALSE).
#'
#'@param allow_out_of_range Whether to allow indices out of range (simply disregard them) or to stop if indices out of range are found.    
#'
#'@return Vector of character strings with the names of the variables in the NetCDF file.
#'
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
NcToArray <- function(file_to_read, dim_indices = NULL, vars_to_read = NULL,
                      drop_var_dim = FALSE, unlist = TRUE, 
                      expect_all_indices = FALSE, allow_out_of_range = TRUE) {
  file_opener <- NcOpen
  file_closer <- NcClose
  file_dim_reader <- NcReadDims
  is_single_na <- function(x) ifelse(length(x) > 1, FALSE, is.na(x))
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

  var_tag <- 'var'
  # Check dim_indices
  if (length(dim_indices) == 0) {
    dim_indices <- NULL
  }
  if (!is.null(dim_indices)) {
    if (!is.list(dim_indices)) {
      stop("Parameter 'dim_indices' must be a list of numeric vectors.")
    }
    if (is.null(names(dim_indices))) {
      stop("Parameter 'dim_indices' must have dimension names as names.")
    }
    var_indices_position <- NULL
    i <- 1
    while (i <= length(dim_indices)) {
      if (names(dim_indices)[i] %in% c('var', 'variable')) {
        vars_to_read <- dim_indices[[i]]
        var_indices_position <- i
        var_tag <- names(dim_indices)[i]
      } else {
        if (!(names(dim_indices)[i] %in% names(file_object$dim))) {
          stop("Provided indices in 'dim_indices' for a non-existing dimension.")
        }
        if (!(is.numeric(dim_indices[[i]]) || is.logical(dim_indices[[i]]))) {
          stop("Parameter 'dim_indices' must be a list of numeric vectors, TRUE or NA.")
        }
      }
      if (identical(dim_indices[[i]], TRUE)) {
        dim_indices[[i]] <- NA
      }
      i <- i + 1
    }
    if (!is.null(var_indices_position)) {
      dim_indices <- dim_indices[-var_indices_position]
    }
  }

  # Check vars_to_read
  if (is.character(vars_to_read) || is.numeric(vars_to_read)) {
    vars_to_read <- list(vars_to_read)
  }
  print_error <- FALSE
  if (is.list(vars_to_read)) {
    if (!all(sapply(vars_to_read, function(x) is.character(x) || is.numeric(x)))) {
      print_error <- TRUE
    }
  } else {
    print_error <- TRUE
  }
  if (print_error) {
    stop("The variables to take specified in 'vars_to_read' or in 'dim_indices[['var']]' must be one or a list of numeric vectors or vectors of character strings.")
  }
#  if (!is.character(vars_to_read) && !is.numeric(vars_to_read)) {
#    stop("Parameter 'vars_to_read' must be a numeric vector or vector of character strings.")
#  }
  result_list <- NULL
  for (vars_to_read_vector in vars_to_read) {
    result <- NULL
    if (!is.null(file_object)) {
      # Create all variables that are 'dimvars'
      extra_dimvars <- NULL
      extra_dimvars_list <- NULL
      for (dim_name in names(file_object$dim)) {
        if (file_object$dim[[dim_name]]$create_dimvar) {
          new_var <- list(name = dim_name, ndims = 1, 
                          size = file_object$dim[[dim_name]]$len,
                          units = file_object$dim[[dim_name]]$units,
                          dim = list(file_object$dim[[dim_name]]))
          new_var_extra_atts <- ncatt_get(file_object, dim_name)
          new_var[names(new_var_extra_atts)] <- new_var_extra_atts
          extra_dimvars_list <- c(extra_dimvars_list, setNames(list(new_var), dim_name))
          extra_dimvars <- c(extra_dimvars, dim_name)
        }
      }
      #file_object$var[extra_dimvars] <- extra_dimvars_list
      #file_object$nvars <- file_object$nvars + length(extra_dimvars)
      if (is.numeric(vars_to_read_vector)) {
        if (any(vars_to_read_vector > (length(file_object$var) + length(extra_dimvars)))) {
          stop("Provided numerical variable indices out of bounds in 'vars_to_read'.")
        }
        vars_to_read_vector <- c(sapply(file_object$var, '[[', 'name'), extra_dimvars)[vars_to_read_vector]
      }
      for (var_name in vars_to_read_vector) {
        if (var_name %in% extra_dimvars) {
          indices_to_take <- TRUE
          if (var_name %in% names(dim_indices)) {
            indices_to_take <- dim_indices[[var_name]]
            if (length(dim(indices_to_take)) > 1) {
              stop("More than 1 dimensions found for the dimension variable ", var_name, ".")
            }
          }
          var_result <- file_object$dim[[var_name]]$vals[indices_to_take]
          ## TODO: Crop dimensions in attributes
          #atts <- file_object$dim[[var_name]]
          atts <- extra_dimvars_list[[var_name]]
          atts_to_remove <- c('vals', 'name', 'len', 'group_index', 
                              'group_id', 'id', 'dimvarid', 'create_dimvar')
          if (any(names(atts) %in% atts_to_remove)) {
            atts <- atts[-which(names(atts) %in% atts_to_remove)]
          }
          units <- file_object$dim[[var_name]]$units
          if (is.null(dim(var_result))) {
            dim(var_result) <- length(var_result)
          }
          names(dim(var_result)) <- sapply(extra_dimvars_list[[var_name]]$dim, '[[', 'name')
        } else {
          var_result <- NULL
          found_dims <- file_dim_reader(file_object, var_name)
          if ('var' %in% names(found_dims)) {
            found_dims <- found_dims[-which(names(found_dims) == 'var')]
          }
          if (length(vars_to_read_vector) == 1 && length(vars_to_read) == 1) {
            if (!all(names(dim_indices) %in% names(found_dims))) {
              stop("Missing dimensions in the file.\nExpected: ",
                   paste(names(dim_indices), collapse = ', '), "\n",
                   "Found: ", paste(names(found_dims), collapse = ', '), "\n",
                   file_path)
            }
          }
          indices_to_take <- as.list(rep(NA, length(found_dims)))
          names(indices_to_take) <- names(found_dims)
          extra_dims <- NULL
          common_dims <- which(names(found_dims) %in% names(dim_indices))
          if (length(common_dims) > 0) {
            extra_dims <- found_dims[-common_dims]
            if (length(extra_dims) == 0) {
              extra_dims <- NULL
            }
          } else {
            extra_dims <- found_dims
          }
          if (length(extra_dims) > 0) {
            if (expect_all_indices) {
              if (any(extra_dims != 1)) {
                stop("Unexpected extra dimensions (of length > 1) in the file.\nPossible dims expected: ",
                     paste(names(dim_indices), collapse = ', '), "\n",
                     "Found dims: ", paste(names(found_dims), collapse = ', '), "\n",
                     file_path)
              }
            }
          }
          any_empty_selectors <- FALSE
          # Here we are allowing for indices out of range (simply discarding them).
          for (inner_dim in names(indices_to_take)) {
            if (inner_dim %in% names(dim_indices)) {
              indices_to_take[[inner_dim]] <- dim_indices[[inner_dim]]
            }
            inds_out_of_range <- which(indices_to_take[[inner_dim]] > found_dims[inner_dim])
            if (length(inds_out_of_range) > 0) {
              if (allow_out_of_range) {
                indices_to_take[[inner_dim]] <- indices_to_take[[inner_dim]][-which(indices_to_take[[inner_dim]] > found_dims[inner_dim])]
              } else {
                stop("Provided indices out of range for dimension '", inner_dim, "'.")
              }
            }
            if (length(indices_to_take[[inner_dim]]) == 0) {
              any_empty_selectors <- TRUE
            }
            if (length(which(indices_to_take[[inner_dim]] < 0)) > 0) {
              stop("Invalid indices provided for '", inner_dim, "'.")
            }
          }
          if (!any_empty_selectors) {
            #missing_dims <- NULL
            #if (length(found_dims) < length(indices_to_take)) {
            #  missing_dim_names <- names(dim_indices)[-which(names(dim_indices) %in% names(dims))]
            #  missing_dim_indices <- lapply(missing_dim_names, function(x) dim_indices[[x]])
            #  if (any(!sapply(missing_dim_indices, identical, 1))) {
            #    stop("Could not find all expected dimensions in the file.\nExpected: ",
            #         paste(names(dim_indices), collapse = ', '), "\n",
            #         "Found: ", paste(names(dims), collapse = ', '), "\n",
            #         file_path)
            #  } else {
            #    original_dims <- sapply(dim_indices, length)
            #    names(original_dims) <- names(dim_indices)
            #    dim_indices <- dim_indices[-which(names(dim_indices) %in% missing_dim_names)]
            #  }
            #  missing_dims <- missing_dim_names
            #}
            start <- sapply(indices_to_take, function(x) if (is_single_na(x)) 1 else min(x))
            count <- sapply(indices_to_take, function(x) if (is_single_na(x)) -1 else max(x) - min(x) + 1)
            # Support for character strings
            if ((file_object[['var']][[var_name]][['prec']] == 'char') && 
                (length(file_object[['var']][[var_name]][['dim']]) > 1)) {
              start <- c(1, start)
              count <- c(-1, count)
            ##  original_ncvar_get_inner <- ncvar_get_inner
            ##  assignInNamespace('ncvar_get_inner', .ncvar_get_inner, 'ncdf4')
            }
            var_result <- do.call('[', c(list(ncvar_get(file_object, var_name, start, count, collapse_degen = FALSE)),
                                         lapply(indices_to_take, function(x) if (is_single_na(x)) TRUE else x - min(x) + 1), list(drop = FALSE)))
            ### Support for character strings
            ##if ((file_object[['var']][[var_name]][['prec']] == 'char') && 
            ##    (length(file_object[['var']][[var_name]][['dim']]) > 1)) {
            ##  assignInNamespace('ncvar_get_inner', original_ncvar_get_inner, 'ncdf4')
            ##}
            #metadata <- c(metadata, structure(list(file_object$var[[var_name]]), .Names = var_name))
            names(dim(var_result)) <- names(indices_to_take)
            # Drop extra dims
            if (!is.null(extra_dims) && expect_all_indices) {
              reduced_dims <- dim(var_result)[-which(names(indices_to_take) %in% names(extra_dims))]
              if (length(reduced_dims) > 0) {
                dim(var_result) <- reduced_dims
              } else {
                dim(var_result) <- NULL
              }
            }
            # Reorder if needed
            reorder_back <- NULL
            indices_dims <- names(dim_indices)[which(names(dim_indices) %in% names(dim(var_result)))]
            if (length(indices_dims) > 0) {
              if (any(setdiff(names(dim(var_result)), names(extra_dims)) != indices_dims)) {
                reorder_back <- 1:length(dim(var_result))
                dims_to_reorder <- which(!(names(dim(var_result)) %in% names(extra_dims)))
                reorder_back[dims_to_reorder] <- dims_to_reorder[sapply(indices_dims, 
                                                                   function(x) {
                                                                     which(names(dim(var_result))[dims_to_reorder] == x)
                                                                   })]
                dimname_bk <- names(dim(var_result))
                var_result <- aperm(var_result, reorder_back)
                names(dim(var_result)) <- dimname_bk[reorder_back]
                #indices_to_take <- indices_to_take[reorder]
              }
            }
            ## TODO: Crop dimensions in attributes
            #if (!is.null(missing_dims)) {
            #  dim(var_result) <- original_dims
            #}
            #attr(var_result, 'variables') <- metadata
          }
          atts <- file_object$var[[var_name]]
          atts_to_remove <- c('id', 'name', 'ndims', 'natts', 'size', 
                              'dimids', 'group_index', 'chunksizes', 
                              'storage', 'shuffle', 'compression', 'dims', 
                              'varsize', 'longname')
          if (any(names(atts) %in% atts_to_remove)) {
            atts <- atts[-which(names(atts) %in% atts_to_remove)]
          }
          extra_atts <- ncatt_get(file_object, var_name)
          atts[names(extra_atts)] <- extra_atts
          units <- file_object$var[[var_name]]$units
          #names(dim(var_result)) <- sapply(file_object$var[[var_name]]$dim, '[[', 'name')
        }
        if (!is.null(var_result)) {
          if (!(drop_var_dim && (length(vars_to_read_vector) == 1))) {
            dim(var_result) <- c(setNames(1, var_tag), dim(var_result))
          }
          attr(var_result, 'variables') <- structure(list(atts), .Names = var_name)
          ## TODO: Take the general attributes out of atts and put them as
          ##       global attributes.
          if (is.null(result)) {
            result <- var_result
          } else {
            new_attrs <- c(attr(result, 'variables'), 
                           attr(var_result, 'variables'))
            result <- .MergeArrays(result, var_result, var_tag)
            attr(result, 'variables') <- new_attrs
          }
        }
      }
    }
    if (is.null(result_list)) {
      if (length(vars_to_read) == 1 && unlist) {
        result_list <- result
      } else {
        if (length(vars_to_read_vector) == 1) {
          result_list <- structure(list(result), .Names = vars_to_read_vector)
        } else {
          result_list <- list(result)
        }
      }
    } else {
      if (length(vars_to_read_vector) == 1) {
        result_list <- do.call('[[<-', list(x = result_list, 
                                            i = vars_to_read_vector,
                                            value = result))
      } else {
        result_list <- do.call('[[<-', list(x = result_list, 
                                            i = length(result_list) + 1,
                                            value = result))
      }
    }
  }
  if (close) {
    file_closer(file_object)
  }
  result_list
}

nc2a <- NcToArray
