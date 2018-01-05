#' @title changes the input dataset
#' @description This function edits the input dataset by adding more information about specific
#' products like bulksize. The information comes from a second dataset, productInfo, which needs
#' to have a column containing all names of products that exist in the original dataset.
#' @export
# sort data, add columns, adapt product names, etc ####

startupSettings <- function(dataset, productInfo, reduce = TRUE) {
  
  
  kornumsatz <- dataset
  Position <- 1:nrow(kornumsatz)
  kornumsatz <- cbind(Position, kornumsatz)
  kornumsatz$Tag <- as.Date(kornumsatz$Tag, format="%Y-%m-%d")
  kornumsatz$Produkt <- as.character(kornumsatz$Produkt)
  VPE <- numeric(nrow(kornumsatz))
  kornumsatz <- cbind(kornumsatz, VPE)
  
  
  ### different product names, but same product, summarized ####
  productInfo$Produkte_App <- as.character(productInfo$Produkte_App)
  productInfo$Produkte_Zusammenfassung <- as.character(productInfo$Produkte_Zusammenfassung)
  productInfo$Verpackungseinheit <- as.numeric(productInfo$Verpackungseinheit)

  for (i in 1:nrow(productInfo)) {
    kornumsatz[kornumsatz$Produkt == productInfo[i,"Produkte_App"],]$Produkt <- 
      rep(productInfo[i,"Produkte_Zusammenfassung"], 
          nrow(kornumsatz[kornumsatz$Produkt == productInfo[i,"Produkte_App"], ]))
    kornumsatz[kornumsatz$Produkt == productInfo[i,"Produkte_Zusammenfassung"], ]$VPE <- 
      rep(productInfo[i,"Verpackungseinheit"], 
          nrow(kornumsatz[kornumsatz$Produkt == productInfo[i,"Produkte_Zusammenfassung"], ]))
  }
  
  # correction for beans
  kornumsatz[kornumsatz$Produkt == "Bohnen Borlotti" & 
               kornumsatz$Tag >= as.Date("2016-10-13", origin="1970-01-01"), ]$Produkt <- "Bohnen schwarz"
  
  rm(VPE, i, Position)
  
  kornumsatz$Produkt <- as.factor(kornumsatz$Produkt)
  # one date, two rows -> reduce them to one
  if (reduce == TRUE) kornumsatz <- reduceONE(get("kornumsatz"))
  kornumsatz$Tag <- as.character(kornumsatz$Tag)
  return(kornumsatz)
}