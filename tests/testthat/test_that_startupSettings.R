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
    newDataset <- addInfos(testData, testInfos)
    print(newDataset)
    print(ncol(newDataset))
    expect_is(newDataset, "data.frame")
    expect_length(newDataset,
                  ncol(testData) + 4)
    # expect_named(colnames(newDataset),
    #              c(colnames(testData), "ID", "VPE", "Bestand.Einheit"),
    #              ignore.order = T)
    expect_is(newDataset$ID, "integer")
    expect_is(newDataset$VPE, "numeric")
    expect_is(newDataset$Produkt_App, "character")
    expect_is(newDataset$Produkt_Zusammenfassung, "character")
    expect_false(c(0, NA) %in% newDataset$VPE)
    expect_false(c(0, NA) %in% newDataset$ID)
    expect_gt( # expect greater than
      length(unique(newDataset$Produkt_App)),
      length(unique(newDataset$Produkte_Zusammenfassung))
    )

  }
)