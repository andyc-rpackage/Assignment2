#' fars_read 
#' 
#' This is a simple function to read data contained in a file
#'
#' @param filename a string indicating the name of the file to read the data from
#'
#' @return this function returns a dataframe containing the data stroed in the file
#'
#' @note if the input specified file doesn't exist the function will stop and throw a file not found error
#' 
#' @examples   
#' fn<-make_filename(2013)
#' fars_read(fn)
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df 
#' 
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
#' @param year a string indicating the name of the file to read the data from
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
        system.file("extdata",sprintf("accident_%d.csv.bz2", year),package = "Assignment2")
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
#' @importFrom dplyr mutate select %>%
#' 
#' @examples 
#' v<-c(2014,2015)
#' d<-fars_read_years(v)
#'
#' @export
#'
fars_read_years <- function(years) {
        lapply(years, function(cyear) {
                file <- make_filename(cyear)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate_(dat, quote(YEAR) == cyear) %>% 
                                dplyr::select_(quote(MONTH), quote(YEAR))
                }, error = function(e) {
                        warning("invalid year: ", cyear)
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
#' @importFrom dplyr bind_rows group_by summarize %>%
#' @importFrom tidyr spread 
#' 
#' @examples 
#' v<-c(2014,2015) 
#' d<-fars_summarize_years(v)
#' 
#'
#' @export
#'

fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>% 
                dplyr::group_by_(quote(YEAR), quote(MONTH) ) %>% 
                dplyr::summarize(ntot = n()) %>%
                tidyr::spread_(key_="YEAR",value_="ntot")
}

#' fars_map_state
#' 
#' This function plots the spatial distribution of fatalities corresponding to the input state number and year 
#'
#' @param year a vector of years
#'
#' @param state.num number indicative of the state
#'
#' @return graphical map of the spatial distribution of fatalisties for agive state in a given year 
#'
#' @note if an invalid year or state number is defined in the input a warning message will be thrown
#' 
#' 
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#' 
#' @examples  
#' library(maps)
#' fars_map_state(10,2014)
#'
#' @export
#'
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(eval(quote(STATE),data))))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter(data, eval(quote(STATE)== state.num,data))
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
