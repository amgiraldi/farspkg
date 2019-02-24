#' Reads a dataset into R
#' 
#' This function read an existing file in your directory and convert it into a
#' data table in R. Keep in mind that the file to be imported into R must be in
#' your directory, otherwise you may get an error.
#' 
#' @param  filename - provides the name of the file with the data to be read into R
#' @return a table data frame in R
#' @importFrom readr "read_csv" this function reads the file given as filename into R.
#' Note that the argument "progress" is set automatically to be FALSE.
#'
#' @importFrom dplyr "tbl_df" this function puts the data as a table. 
#' @examples 
#' fars_read(accident_2013)
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


#' Creates the name for data file 
#' 
#' This function creates the name for the data file to be read into R.
#' 
#' @param year the year of the data to be read as integer. Keep in mind that if not given
#' an integer as argument, you may get an error. 
#' @return this function returns a string with the name of the file "accident" and the 
#' year provided as imput.
#' 
#' @examples 
#' make_filename(2014)
#' 
#' @export

make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Reads datafiles into R at once
#' 
#' This function reads one or more datafiles into R at once. 
#' 
#' @param years a vector containing the years for the files to be read into R. Keep in 
#' mind that you must provide the years of data that are available as datasets, otherwise
#' you'll get an error. 
#' @inheritParams make_filename year
#' @inheritParams fars_read file
#' @importFrom dyplyr mutate
#' @importFrom dyplyr select
#' @return a dataframe(s) in R
#' 
#' @examples 
#' fars_read_years(c(2014,2015))
#' 
#' @export
#' 

fars_read_years <- function(years) {
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

#' Provides the sample size for datasets
#' 
#' This function gives the sample size for each dataset imputed into it.
#' 
#'  @param years
#'  @inheritParams fars_read_years
#'  @importFrom dyplyr bind_rows 
#'  @importFrom dyplyr group_by
#'  @importFrom dyplyr summarize
#'  @importFrom tidyr spread
#'  @return a vector containing the sample size by dataset (year)
#'  
#'  @examples 
#'  fars_summarize_years(c(2013,2014,2015))
#'  
#'  @export

fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>% 
    dplyr::group_by(year, MONTH) %>% 
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Plots the points of the accidents reported in the data set by location
#' 
#' This function plots a point at the location where the accidents reported for each year
#' happened. You must provide the year and the state where you want to plot the accidents.
#' It may be the case that there were no accidents reported in a state for a given year. 
#' In that case there will not be any points to plot. Also, the number rovided for the
#' state must be in the desrired range, otherwise you'll get an error if the number given
#' to the function is not associated with a particuar state. 
#' 
#'  @param state.num an integer indicating the id for the state to be plotted
#'  @inheritParams make_filename
#'  @return a map plotting the points at the location of the accdents reported for the 
#'  given state
#'  @importFrom dyplyr filter
#'  @importFrom maps map
#'  @importFrom graphics points
#'  
#'  @examples 
#'  fars_map_state(01,2014)
#'  
#'  @export
#'  

fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)
  
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