#' @title prepare a datatable for shiny
#' @description This function prepares a given dataset for visualization in
#' the foodstorage shiny app. It colors storage values below 20% in orange,
#' below 10% in red and more than 20% are colored in blue.
#' @param kornumsatz kornumsatz data frame which results from 
#' \code{startupSettings()}
#' @param filter character string for filtering the dataset; default is
#' "nothing"; look at details
#' @details Filter can be: "nothing", which means no filter is applied.
#' "available" means only available products are shown. "empty" shows prducts
#' which are empty at this moment.
#' @return A colored datatable is returned.
#' @export


prepareDatatable <- function(kornumsatz, filter = "nothing", test = FALSE){
  
  # load current storage and add a procentual value column
  data <- currentStorage(kornumsatz) %>%
    mutate(Warenbestand.Prozent = Bestand.Einheit / VPE)
  
  if (filter == "available") {
    data <- data %>%
      filter(Bestand.Einheit > 0)
  }
  if (filter == "empty") {
    data <- data %>% 
      filter(Bestand.Einheit <= 0)
  }

  data <- data %>%
    rename(Produktname_App = Produkt) %>%
    rename(Zusammenfassung = Produkt_Zusammenfassung) %>%
    rename(Warenbestand = Bestand.Einheit) %>%
    select(
      Produktname_App, 
      Zusammenfassung, 
      Lieferant, 
      Lieferant2,
      Produktgruppe,
      Warenbestand,
      Einheit,
      Warenbestand.Prozent
    )
  if (test == TRUE) return(data)
  
  # level2nd <- level2ndFUN(kornumsatz, product.group, deliverers)
  # cS <- currentStorage(level2nd$ONEprod, rawlist = T)[c(4,5,6,8,10)]
  # uniqueStarting <- starting_csv[-which(duplicated(starting_csv$Produkte_Zusammenfassung)), -1]
  # uniqueStarting$Produkte_Zusammenfassung <- as.character(uniqueStarting$Produkte_Zusammenfassung)
  # Info.mit.Bestand <- merge(uniqueStarting, cS, by.x="Produkte_Zusammenfassung", by.y = "Produkt")
  # Info.mit.Bestand <- Info.mit.Bestand[,c("Produkte_Zusammenfassung","Bestand.Einheit","Einheit","Produktgruppe","Lieferant","Lieferant2","Preis","Verpackungseinheit")]
  # names(Info.mit.Bestand) <- c("Produkte", "Aktueller Bestand", "Einheit", "Produktgruppe", "Lieferant", "Lieferant2", "Preis", "Verpackungseinheit")
  # Info.mit.Bestand$Bestand.in.prozent <- Info.mit.Bestand$`Aktueller Bestand`/Info.mit.Bestand$Verpackungseinheit
  # alles <- Info.mit.Bestand
  # gibts <- Info.mit.Bestand[which(Info.mit.Bestand$`Aktueller Bestand` > 0),]
  # leer <- Info.mit.Bestand[which(Info.mit.Bestand$`Aktueller Bestand` == 0),]
  # if(was=="alles"){data <- Info.mit.Bestand}
  # if(was=="gibts"){data <- gibts}
  # if(was=="leer"){data <- leer}
  data <- datatable(
    data, 
    selection = "single",
    filter = 'top',
    options=list(columnDefs = list(list(visible = FALSE, targets = 8))),
    caption = 'Bestand weniger als 20 %: orange; weniger als 10 %: rot'
  ) %>%
    formatStyle(
      2, valueColumns = 8, 
      color = JS("value < 0.1 ? 'red' : value > 0.1 & value < 0.2 ? 'orange' : 'blue'")
    )
  
  return(data)
}

# TabelleB(kornumsatz = kornumsatz, was="leer")
### Funktion schreiben (merge von current storage und starting csv)
### export (roxygen2 doku internet)
### Doku schreiben 
### 