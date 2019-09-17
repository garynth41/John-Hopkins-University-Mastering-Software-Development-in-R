#' Read in data from a csv file
#' ## A note from the author:
#' ### Please leave feedback welcomed, thank you.
#'
#'\code{fars_read} returns a data.frame with data from the supplised csv file.
#'
#' The function takes a character vector of a path of a filename to read into R.
#' If the filename does not exist the function will stop and an error will be thrown.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @note fars_read depends on read_csv and tbl_df from the readr and dplyr packages respectively
#'
#' @param filename a character vector of filename of a csv file to read
#'
#' @return this function returns a dataframe of the data read in from a csv file
#'
#' @examples
#' \dontrun{
#'   fars_read("data/filename.csv")
#' }
#'
#'
#' @export
fars_read <- function(filename) {
  # Checks if the file exists
  if(!file.exists(filename))
    # If the file does not exist, then throw an error and notify the user
    stop("file '", filename, "' does not exist")
  # Suppress any messages
  data <- suppressMessages({
    # Use read_csv from the readr package into the environment
    ## Do not display progress bar
    readr::read_csv(filename, progress = FALSE)
  })
  # Use the dplyr package to forward the argument to as.data.frame from the tibble package
  ## Return the dataframe
  dplyr::tbl_df(data)
}



#' Creates a filename based on year input
#'
#'\code{make_filename} will create a character vector filename with an assocaited year input
#'
#' This takes a year input and creates a filename with this year.
#' If the year is does not pass \code{as.integer} it will have a value of NA, and the function
#' will throw an error after being passed to sprintf.
#'
#' @param year The year to be in the name of the file.
#'
#' @note This package does not depends on any extensions
#'
#' @return A character vector filename
#'
#' @examples
#' \dontrun{
#'   make_filename(2013)
#' }
#'
#'
#' @export
make_filename <- function(year) {
  # Attempt to coerce to integer type, this answer will be NA unless the coercion succeeds
  year <- as.integer(year)
  # Return character vector containing formatted combination of text and variable values
  sprintf("accident_%d.csv.bz2", year)
}


#' Read in multiple files by year
#'
#'\code{fars_read_years} takes a vector of years it iterates over them to
#'create filenames to read in and return with only the MONTH and year columns selected
#'
#' The function will create a filename depending on the year input, if the file does not exist an error will be thrown.
#' If it does exist, it will attempt to read them in, mutate a new column with the year and then select
#' the columns MONTH and year.
#'
#' @param years A vector of years to read in
#'
#' @importFrom dplyr mutate select %>%
#'
#' @note this function depends on dplyr mutate and select functions
#'
#' @return returns a list of dataframes with columns MONTH and year, NULL when an error is encountered
#'
#' @examples
#' \dontrun{
#'   fars_read_years(c(2013, 2014))
#' }
#'
#'
#' @export
fars_read_years <- function(years) {
  # Apply a FUN to run on each vector element
  lapply(years, function(year) {
    # Create a filename by passing the year to make_filename
    file <- make_filename(year)
    # Implement condition system with tryCatch
    tryCatch({
      # Read the file into the environment using fars_read
      dat <- fars_read(file)
      # Add another variable with called year with the value year
      ## Updated change from NSE to SE
      dplyr::mutate_(dat, year = year) %>%
        # select only the columns month and year
        ## Updated change from NSE to SE
        dplyr::select_('MONTH', 'year')
      # Condition when error occures to warn the year with year and return NULL
    }, error = function(e) {
      # Warning year
      warning("invalid year: ", year)
      # Return NULL
      return(NULL)
    })
  })
}

#' Summarse data by year
#'
#' \code{fars_summarize_years} takes a list of years and reads them in using the \code{fars_read_years}
#' it then binds these dataframes together and summarises the data.
#'
#' The function will take in a vector of years and read in the data using the fars_summarize_years function,
#' it then binds these rows together and groups by the year and MONTH column creating a count column: n.
#' The data is then converted from a long format to a wide format using the spread function in tidyr.
#'
#' @param years The years to read in and summarise
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#'
#' @note This functions depends on the bind_rows, group_by, summarize functions from dplyr and spread from tidyr
#'
#' @return a data.frame of summarised data which is converted to a wide format
#'
#' @examples
#' \dontrun{
#'   fars_summarize_years(c(2013, 2014))
#' }
#'
#'
#' @export
fars_summarize_years <- function(years) {
  #  Pass the years to fars_read_years to read in a vector of data
  dat_list <- fars_read_years(years)
  # Bind all the rows together
  dplyr::bind_rows(dat_list) %>%
    # Group by the year and Month variable
    ## Updated change from NSE to SE
    dplyr::group_by_('year', 'MONTH') %>%
    # Summarise the data and add a count column called n
    ## Updated change from NSE to SE
    dplyr::summarize_(n = ~n()) %>%
    # Spread the data from long to wide format
    tidyr::spread_('year', 'n')
}

#' Plots points where accidents for given state.num
#'
#' \code{fars_map_state} plots the state given as an integer
#' argument and plots accidents for a given year
#'
#' The function will take a year and state and attempt to plot accidents for a given state number.
#' If there is no data for a state, the function will stop and throw an error. If there are no accidents
#' a message will be returning indicating this.
#'
#' @param state.num state number to plot accidents for
#' @param year year of data to plot accidents that occured
#'
#' @importFrom maps map
#' @importFrom graphics points
#' @importFrom dplyr filter
#'
#' @note This function depends on map and points functions from the maps and graphics package respectively.
#'
#' @return a data.frame of filtered data, if there are no rows then returns invisible(NULL)
#'
#' @examples
#' \dontrun{
#'   fars_map_state(1, 2013)
#' }
#'
#'
#' @export
fars_map_state <- function(state.num, year) {
  # pass the year to make_filename and assign the variable to filename
  filename <- make_filename(year)
  # read in the data of the filename using fars_read
  data <- fars_read(filename)
  # attempt to coerce state.num to integer type
  state.num <- as.integer(state.num)
  # If state.num does not exists in the dat
  #a column STATE
  if(!(state.num %in% unique(data$STATE)))
    # Throw and error and notify
    stop("invalid STATE number: ", state.num)
  # Assign a variable to data.subet that is the data variable filtered for the state.num function argument
  ## Updated change from NSE to SE
  data.sub <- dplyr::filter_(data, 'STATE' == state.num)
  # If there are no rows then message the user there is no accidents for this data
  if(nrow(data.sub) == 0L) {
    # Message
    message("no accidents to plot")
    # Return a temporary invisible copy of NULL
    return(invisible(NULL))
  }
  # Deal with na values accordingly in the LONGITUD AND LATITUDE COLUMNS
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  # Evaluate environment constructed from data
  with(data.sub, {
    # Draw lines and polygons as specified by the data
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    # Draw points
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
