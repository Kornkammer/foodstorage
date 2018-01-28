test_that(
  "prepareDatatable prepares the dt perfect for shiny", {
    #### load dataset ####
    path <- system.file("data", package = "foodstorage")
    files <- list.files(path)
    # filter all backups (files which end up with .BAK)
    backup <- files[which(stringr::str_detect(files, ".BAK$"))]
    appDB <- dbConnect(SQLite(), file.path(path, backup))
    originalData <- dbGetQuery(appDB, '
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
    
    kornDB <- files[which(stringr::str_detect(files, "kornInfo.sqlite"))]
    kornInfo <- dbConnect(SQLite(), file.path(path, kornDB))
    originalInfos <- dbReadTable(kornInfo, "productInfo")
    dbDisconnect(kornInfo)
    
    kornumsatz <- startupSettings(originalData, originalInfos)
    
    #### test prepareDatatable ####
    # print(prepareDatatable(kornumsatz))
    testData <- prepareDatatable(kornumsatz, test = T) %>%
      arrange(Zusammenfassung)

    testData2 <- kornumsatz %>%
      arrange(Produkt_Zusammenfassung)
    expect_equal(
      unique(testData$Zusammenfassung),
      unique(testData2$Produkt_Zusammenfassung)
    )
    expect_equal(
      colnames(testData),
      c("Produktname_App", "Zusammenfassung", "Lieferant", "Lieferant2",
        "Produktgruppe", "Warenbestand", "Einheit", "Warenbestand.Prozent")
    )
    ### filter paramter works
    testData <- prepareDatatable(kornumsatz, filter = "available", test = T)
    expect_length(
      testData$Warenbestand[which(testData$Warenbestand <= 0)],
      0
    )
    testData <- prepareDatatable(kornumsatz, filter = "empty", test = T)
    expect_length(
      testData$Warenbestand[which(testData$Warenbestand > 0)],
      0
    )
  }
)