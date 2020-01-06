#'Title: fars_functions
#'fars_read function
#'
#'This is a function that read in existing data files with readr package
#'without showing progress or printing any messages
#'
#'@param filename A character string giving the file name and path
#'
#'@return this function returns a data frame. If file name or path does
#'not exist, it will give a warning message.
#'
#'@examples
#'\dontrun{fars_read("accident_2013.csv")
#'fars_read("C:\mydata\accident_2013.csv")
#'fars_read("http:\\abc.org\accident_2013.csv")}
#'
#'@importFrom readr read_csv
#'@importFrom dplyr tbl_df
#'
#'
#'@export
fars_read <- function(filename) {
if(!file.exists(filename))
  stop("file '", filename, "' does not exist")
data <- suppressMessages({
  readr::read_csv(filename, progress = FALSE)
})
dplyr::tbl_df(data)
}


#'make_filename function
#'
#'this is a function that compiles a character string of filename
#'with specified year as input
#'
#'@param year A number specifying the year of interest
#'
#'@return this function returns a character string of filename starting with
#'"accident_" and then the integer year and then ending with ".csv.bz2"
#'
#'@examples #use examples instead of example then no warning of example doesn't exist
#'\dontrun{make_filename(2013)}
#'
#'@export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#'fars_read_years function
#'
#'this is a function that reads in complied file names with specified years
#'of interest and then returns data by month of the year of interest
#'
#'@param years A list of numbers specifying the years of interest
#'
#'@return this function returns a data frame. If the year of interest
#'doesn't exist, it will print a warning message.
#'
#'@examples
#'\dontrun{fars_read_years(2013)}
#'
#'@import magrittr
#'@importFrom dplyr mutate
#'@importFrom dplyr select
#'
#'@export
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

#'fars_summarize_years function
#'
#'this is a function that combines data of different years and then
#'creates a summary table of total number of observations by year and month
#'
#'@param years A list of numbers specifying the years of interest
#'
#'@return this function returns a summary table of total number of observations by year and month
#'
#'@examples
#'\dontrun{fars_summarize_years(2013)}
#'
#'@import magrittr
#'@importFrom dplyr bind_rows group_by summarize
#'@importFrom tidyr spread
#'
#'@export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}


#'fars_map_state function
#'
#'this is a function that creates a map of a specified state
#'at a specified year
#'
#'@param state.num A number indicating a state of interest
#'@param year A number of year of interest
#'
#'@return this function returns a map. If state number does not exist,
#'a warning message would print. If no accidents in the state in the year,
#'no plot would be generated and a message would print.
#'
#'@examples
#'\dontrun{fars_map_state(1,2013)}
#'
#'@importFrom dplyr filter
#'@importFrom maps map
#'@importFrom graphics points
#'
#'@export
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
