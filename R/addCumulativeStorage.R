#' @importFrom magrittr '%>%'

addCumulativeStorage <- function(data, duplicates, quantity) {
  
  # 1) replace Menge with MengeNeu from editDataset()
  # 2) add cumulative sum of storage
  newdata <- data %>%
    dplyr::filter(!ID %in% duplicates) %>%
    # for right replacement, arrange data
    dplyr::arrange(Produkt_Zusammenfassung, ID) %>% 
    dplyr::mutate(Menge = quantity$MengeNeu) %>%
    dplyr::group_by(Produkt_Zusammenfassung) %>%
    dplyr::mutate(Bestand.Einheit = round(ave(Menge, FUN = cumsum), 3)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(ID)
  
  return(newdata)
}