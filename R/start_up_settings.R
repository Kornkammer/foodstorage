#' @export
# sort data, add columns, adapt product names, etc ####

startup.settings <- function(table, importPRODUCTS, reduce = TRUE) {
  
  
  kornumsatz <- table
  Position <- 1:nrow(kornumsatz)
  kornumsatz <- cbind(Position, kornumsatz)
  kornumsatz$Tag <- as.Date(kornumsatz$Tag, format="%Y-%m-%d")
  kornumsatz$Produkt <- as.character(kornumsatz$Produkt)
  VPE <- numeric(nrow(kornumsatz))
  kornumsatz <- cbind(kornumsatz, VPE)
  
  
  ### different product names, but same product, summarized ####
  starting.csv <- importPRODUCTS
  starting.csv$Produkte_App <- as.character(starting.csv$Produkte_App)
  starting.csv$Produkte_Zusammenfassung <- as.character(starting.csv$Produkte_Zusammenfassung)
  starting.csv$Verpackungseinheit <- as.numeric(starting.csv$Verpackungseinheit)

  for (i in 1:nrow(starting.csv)) {
    kornumsatz[kornumsatz$Produkt == starting.csv[i,"Produkte_App"],]$Produkt <- 
      rep(starting.csv[i,"Produkte_Zusammenfassung"], 
          nrow(kornumsatz[kornumsatz$Produkt == starting.csv[i,"Produkte_App"], ]))
    kornumsatz[kornumsatz$Produkt == starting.csv[i,"Produkte_Zusammenfassung"], ]$VPE <- 
      rep(starting.csv[i,"Verpackungseinheit"], 
          nrow(kornumsatz[kornumsatz$Produkt == starting.csv[i,"Produkte_Zusammenfassung"], ]))
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