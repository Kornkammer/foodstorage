test_that(
  "startupSettings edits dataset correctly", {
    #### load dataset ####
    path <- system.file("data", package = "foodstorage")
    files <- list.files(path)
    # filter all backups (files which end up with .BAK)
    backup <- files[which(stringr::str_detect(files, ".BAK$"))]
    appDB <- dbConnect(SQLite(), file.path(path, backup))
    testData <- dbGetQuery(appDB, '
SELECT strftime(\'%d/%m/%Y\',transactions.start/1000,\'unixepoch\') AS Tag,
      ROUND(SUM(transaction_products.quantity), 3) AS Menge, 
      transaction_products.unit AS Einheit,
      ROUND(transaction_products.price, 2) AS Preis, 
      transaction_products.title AS Produkt,
      ROUND(SUM(transaction_products.quantity * transaction_products.price), 3) AS Summe
FROM transaction_products 
LEFT JOIN transactions
      ON transactions._id = transaction_products.transaction_id 
WHERE transactions.status IS \'final\' AND transaction_products.account_guid IS \'lager\'
GROUP BY Tag, Produkt, Preis
ORDER BY transactions.start
'
    )
    dbDisconnect(appDB)
    
    kornDB <- files[which(stringr::str_detect(files, ".sqlite$"))]
    kornInfo <- dbConnect(SQLite(), file.path(path, kornDB))
    testInfos <- dbReadTable(kornInfo, "productInfo")
    dbDisconnect(kornInfo)

    #### add two columns: position and bulksize and check this ####
    resultEditDataset <- editDataset(testData, testInfos, test = TRUE)
    newDataset <- resultEditDataset$editData
    expect_is(newDataset, "data.frame")
    expect_equal(
      ncol(newDataset),
      ncol(testData) + 3
    )
    expect_is(newDataset$ID, "integer")
    expect_is(newDataset$VPE, "numeric")
    expect_is(newDataset$Produkt, "character")
    expect_is(newDataset$Produkt_Zusammenfassung, "character")
    expect_is(newDataset$Tag, "Date")
    expect_false(NA %in% newDataset$VPE)
    expect_false(0 %in% newDataset$ID)
    expect_false(NA %in% newDataset$ID)
    expect_gt( # expect greater than
      length(unique(newDataset$Produkt)),
      length(unique(newDataset$Produkt_Zusammenfassung))
    )
    #### expect no double dates and products
    duplicates <- identifyDuplicates(newDataset)
    summariseQuantity <- resultEditDataset$summariseQuantity
    expect_equal(
      nrow(newDataset),
      length(duplicates) + nrow(summariseQuantity)
    )
    # dataset resulting from editDataset() without ID of duplicates = 
    #   dataset resulting from reomveDuplicates
    oldDataset <- newDataset
    newDataset <- addCumulativeStorage(oldDataset, duplicates, summariseQuantity)
    expect_equal(
      nrow(oldDataset %>% filter(!ID %in% duplicates)),
      nrow(newDataset)
    )
    # after removeDuplicates() are no more duplicates in the data
    testData <- newDataset %>%
      group_by(Produkt_Zusammenfassung, Tag) %>%
      select(Produkt_Zusammenfassung, Tag)
    expect_equal(
      nrow(testData[duplicated(testData), ]), 0
    )
    # after removeDuplicates() Bestand.Einheit is one more column
    expect_equal(
      ncol(newDataset),
      ncol(oldDataset) + 1
    )
    expect_is(newDataset$Bestand.Einheit, "numeric")
    
    #### test wrapper function

  }
)