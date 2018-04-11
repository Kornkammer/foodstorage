#' @title adds two columns to a dataset
#' @description 
#' This function adds row numbers to a dataset which equates an ID. Further a vector containing products' bulksize is added.
#' @importFrom magrittr '%>%'

editDataset <- function(dataset, productInfo) {
  # rename colnames of productInfo, because
  #   1) names need to be shorter
  #   2) one same colname together with 'dataset' for inner_join
  # and ensure that datatypes are the right ones
  
  prodInfo <- productInfo %>%
    dplyr::mutate(VPE = as.numeric(Verpackungseinheit)) %>%
    dplyr::mutate(
      Produkt_Zusammenfassung = as.character(Produkte_Zusammenfassung)
    ) %>%
    dplyr::mutate(Produkt = as.character(Produkte_App)) %>%
    dplyr::select(Produkt, Produkt_Zusammenfassung, Lieferant, Lieferant2, 
                  Produktgruppe, VPE)
  
  editData <- dataset %>%
    dplyr::mutate(
      Tag = as.Date(Tag, format = "%d/%m/%Y", origin = "1970-01-01"),
      Produkt = as.character(Produkt),
      ID = 1:length(Tag)
    ) %>%
    dplyr::inner_join(prodInfo, by = "Produkt")
  
  summariseQuantity <- editData %>%
    dplyr::group_by(Produkt_Zusammenfassung, Tag) %>%
    dplyr::summarise(MengeNeu = sum(Menge))
  
  return(list(
    editData = editData, 
    summariseQuantity = summariseQuantity
  ))
}