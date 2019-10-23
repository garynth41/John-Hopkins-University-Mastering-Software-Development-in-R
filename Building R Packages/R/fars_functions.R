#' This function is created in order to check if the file exists and then make it as a data frame
#' and will throw an error if the file does not exist
#'
#' You can firstly check if the file exists the \code{stop} function
#'
#' @param filename input the name of the file as charater
#'
#' @return fars_read this function returns an input data with data frame format
#'
#' @examples
#' \dontrun{
#'    fars_read(mydata)
#' }
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' This function is to take a year as an input
#' Then create and print a name of a file with format "accident_X.csv.bz2"
#' where X is the input year. It will cause error if the file of the input year does not exist.
#'
#' @param year the input year as integer or string that when the file was generated
#'
#' @return make_filename this function returns a character vector of file name "accident_X.csv.bz2"
#' where X will be replaced by the year
#'
#' @examples
#' \dontrun{
#'    make_filename(2013)
#' }
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' The function is to take a list of years and returns a list of data
#' frames with MONTH and year columns based on data in "accident_X.csv.bz2" files.
#' The files need to be in the working directory (Check function fars_read above!).
#'
#' @param years A list of years of integer or string that when the files were generated
#'
#' @return this function returns a list of data frames with MONTH and year columns
#'
#' @examples
#' \dontrun{
#'    fars_read_years(2013:2015)
#'    fars_read_years(2016) #example of warning (return error!)
#' }
#'
#' @inheritParams fars_read
#'
#' @importFrom dplyr mutate select %>%
#' @importFrom stats setNames
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate_(dat, .dots = setNames(list(~year)), year) %>%
        dplyr::select_(~MONTH, ~year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' The function is to take a list of years then use function fars_read_years to return a list of data
#' frames with MONTH and year columns based on data in "accident_X.csv.bz2" files.
#' Then it will return the data frame consists of total number of accidents for each month
#' The files need to be in the working directory (Check function fars_read above!).
#'
#' @param years A list of years of integer or string that when the files were generated
#'
#' @return this function returns a data frame with total number of accidents for each month
#' which is converted to a wide format
#'
#' @examples
#' \dontrun{
#'    fars_read_years(2013:2015)
#'    fars_read_years(2016) #example of warning (return error!)
#' }
#'
#' @inheritParams fars_read_years
#'
#' @importFrom dplyr bind_rows group_by summarize %>%
#' @importFrom tidyr spread
#' @importFrom stats setNames
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by_(~year, ~MONTH) %>%
    dplyr::summarize_(.dots = setNames(~n(), "n")) %>%
    tidyr::spread_(~year, ~n)
}

#' The function is to take a number of a state in US and a year then filter the data, and
#' plot a map of all the accidents in that state. The input state number must be in the data! (correct state)
#' The files need to be in the working directory (Check function fars_read above!).
#'
#' @param state.num a number of a state in US as integer
#' @param year an input year as integer
#'
#' @return this function returns a map with total number of accidents in the input state number
#'
#' @examples
#' \dontrun{
#'    fars_map_state(45, 2015)
#'    fars_read_years(2016) #example of warning (return error!)
#'    fars_read_years(60, 2016) #example of warning (return error!)
#' }
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter_(data, ~STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
