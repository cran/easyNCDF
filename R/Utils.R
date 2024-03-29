.message <- function(...) {
  # Function to use the 'message' R function with our custom settings
  # Default: new line at end of message, indent to 0, exdent to 3, 
  #  collapse to \n*
  args <- list(...)

  ## In case we need to specify message arguments
  if (!is.null(args[["appendLF"]])) {
    appendLF <- args[["appendLF"]]
  } else {
    ## Default value in message function
    appendLF <- TRUE
  } 
  if (!is.null(args[["domain"]])) {
    domain <- args[["domain"]]
  } else {
    ## Default value in message function
    domain <- NULL
  }
  args[["appendLF"]] <- NULL
  args[["domain"]] <- NULL

  ## To modify strwrap indent and exdent arguments
  if (!is.null(args[["indent"]])) {
    indent <- args[["indent"]]
  } else {
    indent <- 0
  }
  if (!is.null(args[["exdent"]])) {
    exdent <- args[["exdent"]]
  } else {
    exdent <- 3
  }
  args[["indent"]] <- NULL
  args[["exdent"]] <- NULL

  ## To modify paste collapse argument
  if (!is.null(args[["collapse"]])) {
    collapse <- args[["collapse"]]
  } else {
    collapse <- "\n*"
  }
  args[["collapse"]] <- NULL

  ## Message tag
  if (!is.null(args[["tag"]])) {
    tag <- args[["tag"]]
  } else {
    tag <- "* "
  }
  args[["tag"]] <- NULL

  message(paste0(tag, paste(strwrap(
    args, indent = indent, exdent = exdent
    ), collapse = collapse)), appendLF = appendLF, domain = domain)
}

.warning <- function(...) {
  # Function to use the 'warning' R function with our custom settings
  # Default: no call information, indent to 0, exdent to 3, 
  #  collapse to \n
  args <- list(...)

  ## In case we need to specify warning arguments
  if (!is.null(args[["call."]])) {
    call <- args[["call."]]
  } else {
    ## Default: don't show info about the call where the warning came up
    call <- FALSE
  }
  if (!is.null(args[["immediate."]])) {
    immediate <- args[["immediate."]]
  } else {
    ## Default value in warning function
    immediate <- FALSE
  }
  if (!is.null(args[["noBreaks."]])) {
    noBreaks <- args[["noBreaks."]]
  } else {
    ## Default value warning function
    noBreaks <- FALSE
  }
  if (!is.null(args[["domain"]])) {
    domain <- args[["domain"]]
  } else {
    ## Default value warning function
    domain <- NULL
  }
  args[["call."]] <- NULL
  args[["immediate."]] <- NULL
  args[["noBreaks."]] <- NULL
  args[["domain"]] <- NULL

  ## To modify strwrap indent and exdent arguments
  if (!is.null(args[["indent"]])) {
    indent <- args[["indent"]]
  } else {
    indent <- 0
  }
  if (!is.null(args[["exdent"]])) {
    exdent <- args[["exdent"]]
  } else {
    exdent <- 3
  }
  args[["indent"]] <- NULL
  args[["exdent"]] <- NULL

  ## To modify paste collapse argument
  if (!is.null(args[["collapse"]])) {
    collapse <- args[["collapse"]]
  } else {
    collapse <- "\n!"
  }
  args[["collapse"]] <- NULL

  ## Warning tag
  if (!is.null(args[["tag"]])) {
    tag <- args[["tag"]]
  } else {
    tag <- "! Warning: "
  }
  args[["tag"]] <- NULL

  warning(paste0(tag, paste(strwrap(
    args, indent = indent, exdent = exdent
    ), collapse = collapse)),  call. = call, immediate. = immediate, 
    noBreaks. = noBreaks, domain = domain)
}

# This function is a helper for the function .MergeArrays.
# It expects as inputs two named numeric vectors, and it extends them
# with dimensions of length 1 until an ordered common dimension
# format is reached.
# The first output is dims1 extended with 1s.
# The second output is dims2 extended with 1s.
# The third output is a merged dimension vector. If dimensions with
# the same name are found in the two inputs, and they have a different
# length, the maximum is taken.
.MergeArrayDims <- function(dims1, dims2) {
  new_dims1 <- c()
  new_dims2 <- c()
  while (length(dims1) > 0) {
    if (names(dims1)[1] %in% names(dims2)) {
      pos <- which(names(dims2) == names(dims1)[1])
      dims_to_add <- rep(1, pos - 1)
      if (length(dims_to_add) > 0) {
        names(dims_to_add) <- names(dims2[1:(pos - 1)])
      }
      new_dims1 <- c(new_dims1, dims_to_add, dims1[1])
      new_dims2 <- c(new_dims2, dims2[1:pos])
      dims1 <- dims1[-1]
      dims2 <- dims2[-c(1:pos)]
    } else {
      new_dims1 <- c(new_dims1, dims1[1])
      new_dims2 <- c(new_dims2, 1)
      names(new_dims2)[length(new_dims2)] <- names(dims1)[1]
      dims1 <- dims1[-1]
    }
  }
  if (length(dims2) > 0) {
    dims_to_add <- rep(1, length(dims2))
    names(dims_to_add) <- names(dims2)
    new_dims1 <- c(new_dims1, dims_to_add)
    new_dims2 <- c(new_dims2, dims2)
  }
  list(new_dims1, new_dims2, pmax(new_dims1, new_dims2))
}

# This function takes two named arrays and merges them, filling with
# NA where needed.
# dim(array1)
#          'b'   'c'         'e'   'f'
#           1     3           7     9
# dim(array2)
#    'a'   'b'         'd'         'f'   'g'
#     2     3           5           9     11
# dim(.MergeArrays(array1, array2, 'b'))
#    'a'   'b'   'c'   'e'   'd'   'f'   'g'
#     2     4     3     7     5     9     11
.MergeArrays <- function(array1, array2, along) {
  if (!(is.null(array1) || is.null(array2))) {
    if (!(identical(names(dim(array1)), names(dim(array2))) &&
        identical(dim(array1)[-which(names(dim(array1)) == along)],
                  dim(array2)[-which(names(dim(array2)) == along)]))) {
      new_dims <- .MergeArrayDims(dim(array1), dim(array2))
      dim(array1) <- new_dims[[1]]
      dim(array2) <- new_dims[[2]]
      for (j in 1:length(dim(array1))) {
        if (names(dim(array1))[j] != along) {
          if (dim(array1)[j] != dim(array2)[j]) {
            if (which.max(c(dim(array1)[j], dim(array2)[j])) == 1) {
              na_array_dims <- dim(array2)
              na_array_dims[j] <- dim(array1)[j] - dim(array2)[j]
              na_array <- array(dim = na_array_dims)
              array2 <- abind::abind(array2, na_array, along = j)
              names(dim(array2)) <- names(na_array_dims)
            } else {
              na_array_dims <- dim(array1)
              na_array_dims[j] <- dim(array2)[j] - dim(array1)[j]
              na_array <- array(dim = na_array_dims)
              array1 <- abind::abind(array1, na_array, along = j)
              names(dim(array1)) <- names(na_array_dims)
            }
          }
        }
      }
    }
    if (!(along %in% names(dim(array2)))) {
      stop("The dimension specified in 'along' is not present in the ",
           "provided arrays.")
    }
    array1 <- abind::abind(array1, array2, along = which(names(dim(array1)) == along))
    names(dim(array1)) <- names(dim(array2))
  } else if (is.null(array1)) {
    array1 <- array2
  }
  array1
}

# This function also exists in startR::Utils. It serves the same functionality as ClimProjDiags::Subset.
.subset <- function(x, along, indices, drop = FALSE) {
  # x: array with dimension names
  # along: a vector of all characters or all numerics
  # indices: a list of indices
  # drop: F/"selected"

  if (any(is.character(along))) {
    along <- match(along, names(dim(x)))
    if (!all(is.numeric(along))) {
      stop("Check parameter 'along'.")
    }
  }
  if (!is.list(indices)) {
    if (length(along) == 1) {
      indices <- list(indices)
    } else {
      stop("Parameter 'indices' should be a list.")
    }
  }
  if (length(indices) != length(along)) {
    stop("Parameter 'along' and 'indices' should have the same length.")
  }
  if (!drop %in% c(FALSE, 'selected')) {
    stop("Parameter 'drop' can only be FALSE or 'selected'.")
  }

  # Save attributes except 'dim'
  saved_attr <- attributes(x)[-which(names(attributes(x)) == 'dim')]

  # Take the subset
  index <- as.list(rep(TRUE, length(dim(x))))
  index[along] <- indices
  subset <- eval(as.call(c(as.name("["), as.name("x"), index, drop = F)))

  if (drop == 'selected') {
    need_remove <- dim(subset)[along] == 1  # T,F
    if (any(need_remove)) {
      dim(subset) <- dim(subset)[-along[which(need_remove)]]
    }
  }

  # Add attributes back
  attributes(subset)[names(saved_attr)] <- saved_attr

  return(subset)
}
