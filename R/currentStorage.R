#' @title filters the current food storage
#' @description This function filters the current food storage. It looks for 
#' the last storage's change for each product and filters them. Optionally
#' one can give a \code{group} as input, which is a character strings 
#' containing product names. If this is given, the function will filter
#' the dataset by these product names.
#' @param dataset kornumsatz as a result from \code{startupSettings()}
#' @param group optional character vector which contians product names
#' @return A dataframe will be returned.
#' @export

currentStorage <- function(dataset, group) {
  
  
  if (!missing(group)) {
    stopifnot(is.character(group))
    
    data <- dataset %>%
      filter(Produkt_Zusammenfassung %in% group) %>%
      group_by(Produkt_Zusammenfassung) %>%
      filter(Tag == last(Tag)) %>%
      ungroup() 
    
  } else {
    data <- dataset %>%
      group_by(Produkt_Zusammenfassung) %>%
      filter(Tag == last(Tag)) %>%
      ungroup()
  }
  
  return(data)
  
}