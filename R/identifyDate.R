#' @title identifies date
#' @description This function identifies the date of the given backup based on 
#' the file name. It separates it by underscores, dots and minus, substracts the 
#' essential numbers and converts it to a date. Optional one can choose the 
#' type and language of the printed date.
#' @param filename character string of the filename which contains a date
#' @param type character string which defines the output, see details; default
#' is "Date"
#' @details There are four defined output types defined at this moment. 
#' \enumerate{
#'   \item \code{"Date"} is from class \code{Date}
#'   \item \code{"numerical_EN"} looks like \code{"01 May 2018"}
#'   \item \code{"character_EN"} looks like \code{"1st of May 2018"}
#'   \item \code{"character_GER"} looks like \code{"1. Mai 2018"}
#' }
#' @return Depending on \code{type}, a \code{Date} or \code{character} will be 
#' returned
#' @examples
#' identifyDate("Knk_2018_05_02--09:53.BAK", type = "character_GER")
#' @export

identifyDate <- function(filename, type = "Date") {
  # split the filename up
  date <- unlist(strsplit(filename, "[_.-]"))
  
  # some security checks whether the filename is still the same
  if ( date[1] != "Knk" |
       length(date) != 7 |
       tail(date, 1) != "BAK" ) 
    stop("Unfortunately the format of the filename probably changed.")
  date <- paste(date[2:4], collapse = "/") 
  date <- as.Date(date, format = "%Y/%m/%d", origin = "1970-01-01")
  
  if (type == "Date") return(date)
  
  # transform date so that it's nicer to read for humans
  numDateEN <- format(date, "%d %B %Y")
  
  if (type == "numerical_EN") return(numDateEN)
  
  # create a conversion tibble
  conversionsDay <- dplyr::tibble(
    dayNum = 1:31,
    dayEN = c("1st", "2nd", "3rd", paste0(4:31, "th")),
    dayGER = paste0(1:31, ".")
  )
  # conversion tibble for months
  conversionsMonth <- dplyr::tibble(
    num = 1:12,
    EN = c("January", "February", "March", "April", "May", "June", "July",
           "August", "September", "October", "November", "December"),
    GER = c("Januar", "Februar", "März", "April", "Mai", "Juni", "Juli",
            "August", "September", "Oktober", "November", "Dezember")
  )
  requestedDay <- lubridate::day(date)
  requestedMonth <- lubridate::month(date)
  splittedDate <- unlist(strsplit(numDateEN, " "))
  if (type == "character_EN") {
    chrDateEN <- paste(
      conversionsDay$dayEN[requestedDay], "of",
      splittedDate[2], splittedDate[3]
    )
    return(chrDateEN)
  }
  
  if (type == "character_GER") {
    chrDateGER <- paste(
      conversionsDay$dayGER[requestedDay],
      conversionsMonth$GER[requestedMonth],
      splittedDate[3]
    )
    return(chrDateGER)
  }
    
}
