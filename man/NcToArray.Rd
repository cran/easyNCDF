\name{NcToArray}
\alias{NcToArray}
\alias{nc2a}
\title{
Read a NetCDF File Into an R Array
}
\description{
Reads one or a set of variables together with metadata items from a single NetCDF file into an R array (see package 'startR' to read data from multiple files/data sets). Indices to retrieve (not necessarily consecutive) can be specified for each of the dimensions. Depending on the format of the request, the variables will be merged in into a single extended array or returned in a list with an array for each variable. The different variables in the file are considered to be stored along a dimension called 'var', so reading a variable 'foo' with dimensions 'lat' and 'lon' would result in an array with the dimensions c('var' = 1, 'lat' = n_lats, 'lon' = n_lons).
}
\usage{
NcToArray(file_to_read, dim_indices = NULL, vars_to_read = NULL,
          drop_var_dim = FALSE, unlist = TRUE,
          expect_all_indices = FALSE, allow_out_of_range = TRUE)
nc2a(file_to_read, dim_indices = NULL, vars_to_read = NULL, 
     drop_var_dim = FALSE, unlist = TRUE,
     expect_all_indices = FALSE, allow_out_of_range = TRUE)
}
\arguments{
  \item{file_to_read}{
Path to the file to be read or a NetCDF object as returned by \code{easyNCDF::NcOpen} or \code{ncdf4::nc_open}. See package 'startR' if need to read data from multiple files/data sets.
  }
  \item{dim_indices}{
Named list with numeric vectors of indices to take for each dimension. The names should correspond to the dimension names which to take the indices for. Non-consecutive indices can be specified. If \code{expect_all_indices = FALSE} (default), it is not mandatory to specify the indices for all (or even any of) the dimensions. In that case all the indices along such dimensions will be read in. If \code{expect_all_indices = TRUE}, then indices for all the dimensions have to be specified for the function to return a data array. In that case, \code{NA} can be used to request all indices for a dimension if desired.
\cr\cr
Since this function considers the variables in a NetCDF file are stored along a 'var' dimension, indices for the (actually non-existing) 'var'/'variable' dimension can be specified. They can be specified in 3 ways:\cr
 - A vector of numeric indices: e.g. \code{list(var = c(1, 3, 5))} to take the 1st, 3rd and 5th found variables.\cr
 - A vector of character strings with variable names: e.g. \code{list(var = c('foo', 'bar'))}.\cr
 - A list of vectors with numeric indices or character strings: e.g. \code{list(var = list(c(1, 3, 'foo'), c(2, 'bar')))}\cr
Vectors with combined numeric indices and character strings are accepted.\cr
Whereas the first two options will return a single extended array with the merged variables, the second option will return a list with an array for each requested variable.
  }
  \item{vars_to_read}{
This parameter is a shortcut to (and has less priority than) specifying the requested variable names via \code{dim_indices = list(var = ...)}. It is useful when all the indices for all the requested variables have to be taken, so the parameter \code{dim_indices} can be skipped, but still only a specific variable or set of variables have to be taken. Check the documentation for the parameter \code{dim_indices} to see the three possible ways to specify this parameter.
  }
  \item{drop_var_dim}{
Whether to drop the 'var' dimension this function assumes (read description). If multiple variables are requested in a vector and \code{unlist = TRUE}, the drop won't be performed (not possible).
  }
  \item{unlist}{
Whether to merge the resulting array variables into a single array if possible (default) or not. Otherwise a list with as many arrays as requested variables is returned.
  }
  \item{expect_all_indices}{
Whether the function should stop if indices are not provided for all the dimensions of any of the requested variables (TRUE) rather than assuming that all the indices are requested for the unspecified dimensions (FALSE). By default the later is done (FALSE).
  }
  \item{allow_out_of_range}{
Whether to allow indices out of range (simply disregard them) or to stop if indices out of range are found.
  }
}
\value{
Array or list of arrays with the data for one or more than one of the requested variables (depending on the parameters). The dimensions are named. The arrays contain the attribute 'variables' with the metadata items found in the NetCDF file.
}
\examples{
# Create an array from R
file_path <- tempfile(fileext = '.nc')
a <- array(1:9, dim = c(member = 3, time = 3))
# Store into a NetCDF twice, as two different variables
ArrayToNc(list(var_1 = a, var_2 = a + 1), file_path)
# Read the dimensions and variables in the created file
fnc <- NcOpen(file_path)
fnc_dims <- NcReadDims(fnc)
var_names <- NcReadVarNames(fnc)
# Read the two variables from the file into an R array
a_from_file <- NcToArray(fnc, vars_to_read = var_names)
NcClose(fnc)
# Check the obtained array matches the original array
print(a)
print(a_from_file[1, , ])

# Example with extra dimensions of length 1
# Creating sample data with singleton dimensions. Only dimensions 'a', 'b' and
# 'c' are of length > 1.
test_var <- array(1:24, dim = c(1, 1, 2, 1, 1, 3, 1, 4, 1, 1))
names(dim(test_var)) <- c('x', 'y', 'a', 'z', 't', 'b', 'u', 'c', 'v', 'w')
# Storing the data into a NetCDF file
a2nc(list(test_var = test_var), file_path)
# Reading the data back
fff <- nc2a(file_path, list(a = NA, b = NA, c = NA), vars_to_read = 'test_var')
# By default, if no indices are provided for the singleton dimensions, they are 
# automatically read in and preserved
dim(fff)
# Reading the data back with expect_all_indices = TRUE
fff <- nc2a(file_path, list(a = NA, b = NA, c = NA), vars_to_read = 'test_var', 
            expect_all_indices = TRUE)
# If indices for all dimensions are not provided and expect_all_indices = TRUE, 
# the function crashes, except if those dimensions are of length 1. In that 
# case, the function ignores those (those are dropped)
dim(fff)
}
\author{
History:\cr
0.0  -  2017-03  (N. Manubens, \email{nicolau.manubens at bsc.es})  -  Original code
}
\keyword{datagen}
