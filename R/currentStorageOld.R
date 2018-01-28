#' @export
currentStorageOld <- function(
  group, 
  horiz = FALSE, 
  fill = TRUE, 
  order.storage = TRUE, 
  rawlist = FALSE, 
  empty = 0.05, 
  result = "current", 
  test_separating = FALSE
) {
  
  big.list <- lapply(group, FUN = prepare, result = result)
  big.df <- do.call(rbind, big.list)
  if (isTRUE(order.storage)) big.df <- big.df[order(big.df$Bestand.Einheit, decreasing = TRUE),]
  if (rawlist == TRUE) return(big.df)
  
  # # separate list: products which are (almost) empty shall not be plotted
  # big.df$Prozent5 <- empty * big.df$VPE
  # if (plot == FALSE) return(big.df)
  # 
  # empty.prods <- big.df[big.df$Bestand_Einheit == 0, ]
  # probably.empty <- big.df[big.df$Bestand_Einheit < big.df$Prozent5 & big.df$Bestand_Einheit != 0, ]
  # big.df <- big.df[big.df$Bestand_Einheit >= big.df$Prozent5, ]
  # 
  # if separating big.df works the number of rows shall be the same. Following for test_that
  if (test_separating == TRUE) {
    rows <- nrow(empty.prods) + nrow(probably.empty) + nrow(big.df)
    return(rows)
  }
  
  # create ordered factor which is necessary for ggplot2
  big.df$Produkt <- factor(big.df$Produkt, levels = big.df$Produkt)
  
  # create ggplot object
  pplot <- ggplot(big.df, aes(x = factor(Produkt), y = Bestand.Einheit)) +
    theme(plot.title = element_text(hjust = 0.5)) + # to center title
    geom_col() + 
    labs(title = "Aktueller Warenbestand", x= "Produkte", y = "Warenbestand in Kilo") +
    guides(fill = FALSE)
  
  # coloured bars or black-white: create ggplot object
  if (fill == TRUE) pplot <- pplot + aes(fill = factor(Produkt))
  
  # horizontal or vertical
  if (horiz == TRUE) pplot <- pplot + coord_flip()
    else {
      pplot <- pplot + 
        theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + 
        coord_cartesian(ylim = c(0,max(big.df$Bestand.Einheit)))
    }
  
  # renderPlot...
  return(pplot)  
}