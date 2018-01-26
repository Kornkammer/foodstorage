addCumulativeStorage <- function(data, duplicates, quantity) {
  
  # 1) replace Menge with MengeNeu from editDataset()
  # 2) add cumulative sum of storage
  newdata <- data %>%
    filter(!ID %in% duplicates) %>%
    arrange(Produkt_Zusammenfassung, Tag) %>% # necessary for right replacement
    mutate(Menge = quantity$MengeNeu) %>%
    group_by(Produkt_Zusammenfassung) %>%
    mutate(Bestand.Einheit = round(ave(Menge, FUN = cumsum), 3)) %>%
    ungroup()
  
  return(newdata)
}