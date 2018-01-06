#' @title adds two columns to a dataset
#' @description 
#' This function adds row numbers to a dataset which equates an ID. Further a vector containing products' bulksize is added.
#' @export

addInfos <- function(dataset, productInfo) {
  # add ID
  dataset$ID <- 1:nrow(dataset)
  
  # rename colnames of productInfo, because
  #   1) names need to be shorter
  #   2) one same colname together with 'dataset' for inner_join
  # and ensure that datatypes are the right ones
  dataset$Produkt <- as.character(dataset$Produkt)
  
  prodInfo <- productInfo %>%
    mutate(VPE = as.numeric(Verpackungseinheit)) %>%
    mutate(Produkt_Zusammenfassung = as.character(Produkte_Zusammenfassung)) %>%
    mutate(Produkt = as.character(Produkte_App)) %>%
    select(Produkt, Produkt_Zusammenfassung, VPE)
  
  newdata <- dataset %>%
    inner_join(prodInfo) %>%
    mutate(ID = 1:length(Menge)) %>%
    group_by(Produkt_Zusammenfassung) %>%
    mutate(Bestand.Einheit = round(ave(Menge, FUN = cumsum), 3)) %>%
    rename(Produkt_App = Produkt)
  return(newdata)
}