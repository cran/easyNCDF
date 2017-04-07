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
      nmv <- numeric_var_indices <- which(is.numeric(vars_to_read_vector))
      if (length(nmv) > 0) {
        if (any(vars_to_read_vector[nmv] > (length(file_object$var) + length(extra_dimvars)))) {
          stop("Provided numerical variable indices out of bounds in 'vars_to_read'.")
        }
        vars_to_read_vector[nmv] <- c(sapply(file_object$var, '[[', 'name'), extra_dimvars)[vars_to_read_vector[nmv]]
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
          if (expect_all_indices) {
            extra_dims <- names(extra_dims)
          } else {
            extra_dims <- NULL
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
            reorder_back <- NULL
            indices_dims <- names(dim_indices)[which(names(dim_indices) %in% names(found_dims))]
            if (length(indices_dims) > 0) {
              if (any(names(found_dims) != indices_dims)) {
                #reorder <- sapply(names(found_dims), function(x) which(indices_dims == x))
                reorder_back <- sapply(indices_dims, function(x) which(names(found_dims) == x))
                #indices_to_take <- indices_to_take[reorder]
              }
            }
            start <- sapply(indices_to_take, function(x) if (is_single_na(x)) 1 else min(x))
            count <- sapply(indices_to_take, function(x) if (is_single_na(x)) -1 else max(x) - min(x) + 1)
            var_result <- do.call('[', c(list(ncvar_get(file_object, var_name, start, count, collapse_degen = FALSE)),
                                         lapply(indices_to_take, function(x) if (is_single_na(x)) TRUE else x - min(x) + 1), list(drop = FALSE)))
            #metadata <- c(metadata, structure(list(file_object$var[[var_name]]), .Names = var_name))
            ## TODO: Crop dimensions in attributes
            if (!is.null(reorder_back)) {
              var_result <- aperm(var_result, reorder_back)
            }
            names(dim(var_result)) <- names(indices_to_take)[reorder_back]
            #if (!is.null(missing_dims)) {
            #  dim(var_result) <- original_dims
            #}
            if (!is.null(extra_dims)) {
              dim(var_result) <- dim(var_result)[-which(names(indices_to_take) %in% extra_dims)]
            }
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
          if (!drop_var_dim || (length(vars_to_read_vector) == 1)) {
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
