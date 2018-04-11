#' @importFrom magrittr '%>%'

identifyDuplicates <- function(data) {
  
  # evaluate IDs of duplicates
  duplicates <- data %>%
    dplyr::group_by(Produkt_Zusammenfassung, Tag) %>%
    dplyr::select(Produkt_Zusammenfassung, Tag, ID)
  duplicates <- duplicates[duplicated(
    duplicates[, c("Produkt_Zusammenfassung", "Tag")]
  ), "ID"]
  duplicates <- unlist(duplicates, use.names = F)
  
  return(duplicates)
  
}