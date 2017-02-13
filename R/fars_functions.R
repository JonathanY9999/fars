#' fars_read
#'
#' Reads a data file given a path.
#'
#' @note A non-existent file path will result in an error.
#'
#' @param filename path to file
#' @import readr
#' @import dplyr
#' @return data.frame
#' @export
#'
#' @examples
#' if(interactive()){
#'    df <- fars_read("path_to_file")
#' }
#'
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' make_filename
#'
#' Creates a file name based on a year.
#'
#' @param year integer
#'
#' @return character string
#'
#' @export
#'
#' @examples
#' if(interactive()){
#'    fp <- make_filename(2013)
#' }
#'
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' fars_read_years
#'
#' Reads in fars files given a vector of years.
#' Returns a data.frame with columns MONTH, year
#' @note Throws a warning and returns NULL when an invalid year is part of the vector.
#'
#'
#' @param years integer vector
#'
#' @return data.frame
#'
#' @import dplyr
#' @export
#'
#' @examples
#' if(interactive()){
#'    df <- fars_read_years(c(2013, 2014, 2015))
#' }
#'
fars_read_years <- function(years) {
  year <- NULL
  MONTH <- NULL
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' fars_summarize_years
#'
#' Summarizes accidents over years and months.
#'
#' @param years integer a vector
#'
#' @import dplyr
#' @import tidyr
#' @return a data frame
#' @export
#'
#' @examples
#' if(interactive()){
#'    fsum <- fars_summarize_years(2013:2015)
#' }
#'
fars_summarize_years <- function(years) {
  year <- NULL
  MONTH <- NULL
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' fars_map_state
#'
#' Maps yearly accidents in a US state as points.
#' @note Throws an error if the numerical id of a state is invalid.
#'
#' @param state.num integer id of US state
#' @param year integer vector of years
#'
#' @import dplyr
#' @import maps
#' @import graphics
#'
#' @return NULL
#' @export
#'
#' @examples
#' if(interactive()){
#'    fsum <- fars_map_state(1, 2013:2015)
#' }
#'
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)
  STATE <- NULL

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
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
