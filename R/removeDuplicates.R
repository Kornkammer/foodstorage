identifyDuplicates <- function(data) {
  
  # evaluate IDs of duplicates
  duplicates <- data %>%
    group_by(Produkt_Zusammenfassung, Tag) %>%
    select(Produkt_Zusammenfassung, Tag, ID)
  duplicates <- duplicates[duplicated(
    duplicates[, c("Produkt_Zusammenfassung", "Tag")]
  ), "ID"]
  duplicates <- unlist(duplicates, use.names = F)
  
  return(duplicates)
  
}