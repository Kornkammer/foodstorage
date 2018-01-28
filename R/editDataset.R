#' @title adds two columns to a dataset
#' @description 
#' This function adds row numbers to a dataset which equates an ID. Further a vector containing products' bulksize is added.
#' @export

editDataset <- function(dataset, productInfo) {
  # rename colnames of productInfo, because
  #   1) names need to be shorter
  #   2) one same colname together with 'dataset' for inner_join
  # and ensure that datatypes are the right ones
  
  prodInfo <- productInfo %>%
    mutate(VPE = as.numeric(Verpackungseinheit)) %>%
    mutate(Produkt_Zusammenfassung = as.character(Produkte_Zusammenfassung)) %>%
    mutate(Produkt = as.character(Produkte_App)) %>%
    select(Produkt, Produkt_Zusammenfassung, Lieferant, Lieferant2, 
           Produktgruppe, VPE)
  
  editData <- dataset %>%
    mutate(Tag = as.Date(Tag, format = "%d/%m/%Y", origin = "1970-01-01")) %>%
    mutate(Produkt = as.character(Produkt)) %>%
    mutate(ID = 1:length(Tag)) %>%
    inner_join(prodInfo)
  
  summariseQuantity <- editData %>%
    group_by(Produkt_Zusammenfassung, Tag) %>%
    summarise(MengeNeu = sum(Menge))
  
  return(list(
    editData = editData, 
    summariseQuantity = summariseQuantity
  ))
}