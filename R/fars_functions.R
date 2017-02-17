#' fars_read 
#' 
#' This is a simple function to read data contained in a file
#'
#' @param filename: a string indicating the name of the file to read the data from
#'
#' @return this function returns a dataframe containing the data stroed in the file
#'
#' @note if the input specified file doesn't exist the function will stop and throw a file not found error
#' 
#' @examples   
#' fars_read("accident_2013.csv.bz2") 
#' fars_read("accident_2014.csv.bz2")
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df 

#' @export
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
#' This is a simple function returns a filename from a year
#'
#' @param year: a string indicating the name of the file to read the data from
#'
#' @return filename corresponding to the input year
#'
#' @examples 
#' make_filename(2014)
#'
#' @export
#'

make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' fars_read_years
#' 
#' This function reads multiple data files identified by their year, one per each element of the the input vector of years
#'
#' @param years a vector of years
#'
#' @return the function returns a list of monthly data one per each year in the input vector 
#'
#' @note if an invalid year is defined in the input vector a warning message will be thrown
#'
#' @importFrom dplyr mutate select 
#' 
#' @examples 
#' v<-c(2014,2015) d<-fars_read_years(v)
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

#' fars_summarize_years
#' 
#' This function summarizes the data corresponding to the input vector of years with monthly aggregate 
#'
#' @param years a vector of years
#'
#' @return a data frame with a summary per each in the input vector of years of agreggated  
#'
#' @note if an invalid year is defined in the input vector a warning message will be thrown
#'
#' @importFrom dplyr bind_rows group_by summarize 
#' @importFrom tidyr spread 
#' 
#' @examples 
#' v<-c(2014,2015) d<-fars_summarize_years(v)
#'
#' @export
#'

fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>% 
                dplyr::group_by(year, MONTH) %>% 
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' fars_map_state
#' 
#' This function plots the spatial distribution of fatalities corresponding to the input state number and year 
#'
#' @param years a vector of years
#'
#' @param state.num number indicative of the state
#'
#' @return graphical map of the spatial distribution of fatalisties for agive state in a given year 
#'
#' @note if an invalid year or state number is defined in the input a warning message will be thrown
#' 
#' @importFrom dplyr filter 
#' @importFrom maps map
#' @importFrom graphics points
#' 
#' @examples  
#' fars_map_state(10,2014)
#'
#' @export
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