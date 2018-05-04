#' @title filters the current food storage
#' @description This function filters the current food storage. It looks for 
#' the last storage's change for each product and filters them. Optionally
#' one can give a \code{group} as input, which is a character string 
#' containing product names. If this is given, the function will filter
#' the dataset by these product names.
#' @param dataset kornumsatz as a result from \code{startupSettings()}
#' @param group optional character vector which contians product names
#' @return A dataframe will be returned.
#' @export
#' @importFrom magrittr '%>%'

currentStorage <- function(dataset, group, dataAreUpToDate = TRUE) {
  # if data aren't up to date use original dataset without product specification
  if (dataAreUpToDate == FALSE) {
    # make sure day is in format 'Date'
    if ( !lubridate::is.Date(dataset$Tag) ) {
      # transform column 'day' to class Date
      data <- dataset %>%
        dplyr::mutate(
          Tag = as.Date(Tag, format = "%d/%m/%Y", origin = "1970-01-01")
        )
    } else {
      data <- dataset
    }
    
    lastBookingOfEachProduct <- data %>%
      dplyr::group_by(Produkt) %>%
      dplyr::slice(n())
    
    newData <- data %>%
      dplyr::group_by(Produkt) %>%
      dplyr::mutate(Warenbestand = cumsum(Menge)) %>%
      dplyr::right_join(
        lastBookingOfEachProduct, by = c("Tag", "Einheit", "Preis", "Produkt")
      ) %>%
      dplyr::select(Tag, Produkt, Preis, Warenbestand, Einheit) %>%
      dplyr::arrange(dplyr::desc(Warenbestand))
    return(newData)
  }
  
  if (!missing(group)) {
    stopifnot(is.character(group))
    
    data <- dataset %>%
      dplyr::filter(Produkt_Zusammenfassung %in% group) %>%
      dplyr::group_by(Produkt_Zusammenfassung) %>%
      dplyr::filter(Tag == last(Tag)) %>%
      dplyr::ungroup() 
    
  } else {
    data <- dataset %>%
      dplyr::group_by(Produkt_Zusammenfassung) %>%
      dplyr::filter(Tag == dplyr::last(Tag)) %>%
      dplyr::ungroup()
  }
  
  return(data)
  
}