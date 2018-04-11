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
#' @importFrom magrittr '%>%'


prepareDatatable <- function(kornumsatz, filter = "nothing", test = FALSE){
  
  # load current storage and add a procentual value column
  data <- dplyr::mutate(
    currentStorage(kornumsatz), 
    Warenbestand.Prozent = Bestand.Einheit / VPE
  ) 
  
  if (filter == "available") {
    data <- dplyr::filter(data, Bestand.Einheit > 0)
  }
  if (filter == "empty") {
    data <- dplyr::filter(data, Bestand.Einheit <= 0)
  }

  data <- data %>%
    dplyr::rename(Produktname_App = Produkt) %>%
    dplyr::rename(Zusammenfassung = Produkt_Zusammenfassung) %>%
    dplyr::rename(Warenbestand = Bestand.Einheit) %>%
    dplyr::select(
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
  
  data <- DT::datatable(
    data, 
    selection = "single",
    filter = 'top',
    options = list(columnDefs = list(list(visible = FALSE, targets = 8))),
    caption = 'Bestand weniger als 20 %: orange; weniger als 10 %: rot'
  )
  
  if (requireNamespace("htmlwidgets", quietly = TRUE)) {
    data <- DT::formatStyle(
      table = data,
      2, valueColumns = 8, 
      color = htmlwidgets::JS(
        "value < 0.1 ? 'red' : value > 0.1 & value < 0.2 ? 'orange' : 'blue'"
      )
    )
  } else {
    warning("If you install 'htmlwidgets', the table will be styled.")
  }
  
  return(data)
} 