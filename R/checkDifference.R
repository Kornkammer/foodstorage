#' @title check difference in data sets
#' @description This function checks if all the products of the given dataset are already known.
#' @export

checkDifference <- function(dataset, productInfo) {
  # check out if there are any new products
  kornumsatz <- dataset
  dif <- dplyr::setdiff(unique(kornumsatz$Produkt), 
                        unique(productInfo$Produkte_App))
  
  return(dif)
}