#' @title adds two columns to a dataset
#' @description 
#' This function adds row numbers to a dataset which equates an ID. Further a vector containing products' bulksize is added.
#' @export

editDataset <- function(dataset, productInfo, test = FALSE) {
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
  
  editData <- dataset %>%
    mutate(Tag = as.Date(Tag, format = "%d/%m/%Y", origin = "1970-01-01")) %>%
    mutate(ID = 1:length(Menge)) %>%
    inner_join(prodInfo)
  
  summariseQuantity <- editData %>%
    group_by(Produkt_Zusammenfassung, Tag) %>%
    summarise(MengeNeu = sum(Menge))
  
  if (isTRUE(test)) return(list(
    editData = editData, 
    summariseQuantity = summariseQuantity
  ))
  
  # identify duplicates of Produkt_Zusammenfassung & Tag, which means
  # for the same product name exists more than one row on one day
  duplicates <- identifyDuplicates(newDataset)
  
  # replace Menge with summariseQuantity and add a cumulative storage
  # column
  newData <- addCumulativeStorage(editData, duplicates, summariseQuantity)
  
  return(newData)
}