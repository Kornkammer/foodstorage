#' @title calculate the quantities of the products per year.
#' @description 
#' from the data kornumsatz, which has the columns "Tag", "Menge", "Produkt" and the data productInfo, with the columns "Produkte_App", "Produkte_Zusammenfassung", the kornumsatz (quantity of products) for every product and year. 
#' @importFrom magrittr %>%
#' @examples 
#' turnoverPerYear(kornumsatz, productInfo,  Produkte_Zusammenfassung)
#' @export

turnoverPerYear <- function(
  kornumsatz, productInfo, group_var
){
  group_var <- dplyr::enquo(group_var)
  kornumsatz_new <- kornumsatz %>% 
    dplyr::left_join(
      productInfo[, c("Produkte_App", "Produkte_Zusammenfassung", "Lieferant")], 
      by=c("Produkt" = "Produkte_App")
    ) %>% 
    dplyr::mutate(Tag = as.Date(Tag, format = "%d/%m/%Y")) %>% 
    dplyr::filter(Menge > 0) %>% 
    dplyr::mutate(Jahr = lubridate::year(Tag)) %>% 
    dplyr::group_by(!!group_var, Jahr) %>% 
    dplyr::summarise(Umsatz = sum(Menge))
  return(kornumsatz_new)
}
#year(kornumsatz$Tag)
## pro Jahr
## nur positive Mengen!
