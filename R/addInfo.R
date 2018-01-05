#' @title adds two columns to a dataset
#' @description 
#' This function adds row numbers to a dataset which equates an ID. Further a vector containing products' bulksize is added.
#' @export

addInfos <- function(dataset, productInfo) {
  # add ID
  dataset$ID <- 1:nrow(dataset)
  
  # ensure that datatypes are the right ones
  dataset$Produkt <- as.character(dataset$Produkt)
  productInfo$Produkte_App <- as.character(productInfo$Produkte_App)
  productInfo$Produkte_Zusammenfassung <- 
    as.character(productInfo$Produkte_Zusammenfassung)
  productInfo$Verpackungseinheit <- as.numeric(productInfo$Verpackungseinheit)
  
  newdata <- dataset
  newdata$VPE <- numeric(nrow(newdata))
  ### add bulksize, called VPE, to dataset ####
  for (i in 1:nrow(productInfo)) {
    newdata[newdata$Produkt == productInfo[i,"Produkte_App"],]$Produkt <- 
      rep(productInfo[i,"Produkte_Zusammenfassung"], 
          nrow(newdata[newdata$Produkt == productInfo[i,"Produkte_App"], ]))
    newdata[newdata$Produkt == productInfo[i,"Produkte_Zusammenfassung"], ]$VPE <- 
      rep(productInfo[i,"Verpackungseinheit"], 
          nrow(newdata[newdata$Produkt == productInfo[i,"Produkte_Zusammenfassung"], ]))
  }
  print(dplyr::setdiff(dataset[,c("Tag", "Produkt")], newdata[, c("Tag", "Produkt")]))
}