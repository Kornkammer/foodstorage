#' @title changes the input dataset
#' @description This function edits the input dataset by adding more information about specific
#' products like bulksize. The information comes from a second dataset, productInfo, which needs
#' to have a column containing all names of products that exist in the original dataset.
#' @export
# sort data, add columns, adapt product names, etc ####

startupSettings <- function(dataset, productInfo) {
  tmpData <- editDataset(dataset, productInfo)
  
  editData <- tmpData$editData
  summariseQuantity <- tmpData$summariseQuantity
  
  # identify duplicates of Produkt_Zusammenfassung & Tag, which means
  # for the same product name exists more than one row on one day
  duplicates <- identifyDuplicates(editData)
  
  # replace Menge with summariseQuantity and add a cumulative storage
  # column
  newData <- addCumulativeStorage(editData, duplicates, summariseQuantity)
  
}