test_that(
  "importData() imports data correctly", {
    #### load dataset ####
    path <- system.file("data", package = "foodstorage")
    files <- list.files(path)
    # filter all backups (files which end up with .BAK)
    backups <- files[which(stringr::str_detect(files, ".BAK$"))]
    current_backup <- backups[length(backups)] # use newest backup
    pathToBackup <- file.path(path, current_backup)
      # a current backup should exist
    expect_length(current_backup, 1)
    # test result of file.path -> if file doesn't exist, length(result) will be 0
    kornInfoExists <- files[which(stringr::str_detect(files, "kornInfo.sqlite"))]
    pathToKornInfo <- file.path(path, kornInfoExists)
    kornInfoNotExists <- files[which(stringr::str_detect(files, 
                                                         "emptyKornInfo.sqlite"))]
      # kornInfo.sqlite should exist, emptyKornInfo.sqlite shouldn't
    expect_length(kornInfoExists, 1)
    expect_length(kornInfoNotExists, 0)
    
    #### two scenaries: 1) There is no kornumsatz_origin before importData()
    kornInfo <- dbConnect(SQLite(), pathToKornInfo)
    tablesKornInfo <- dbListTables(kornInfo)
    if ("kornumsatz_origin" %in% tablesKornInfo) {
      # delete kornumsatz_origin if it exists
      dbSendStatement(kornInfo, "DROP TABLE kornumsatz_origin")
    }
    dbDisconnect(kornInfo)
    expect_false("kornumsatz_origin" %in% tablesKornInfo)
    
    # after importData(), kornumsatz_origin should exist
    TO <- list(
      nameTable = "kornumsatz_origin",
      path = pathToKornInfo
    )
    importData(FROM = pathToBackup, TO = TO)
    
    kornInfo <- dbConnect(SQLite(), pathToKornInfo)
    tablesKornInfo <- dbListTables(kornInfo)
    expect_true("kornumsatz_origin" %in% tablesKornInfo)
    
    # originalData from backup.BAK should be the same as kornumsatz_origin
    backupDB <- dbConnect(SQLite(), pathToBackup)
    originalData <- dbGetQuery(backupDB, '
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
    dbDisconnect(backupDB)
    
    compareData <- dbReadTable(kornInfo, "kornumsatz_origin")
    dbDisconnect(kornInfo)
    
    expect_equal(
      originalData,
      compareData
    )
    #### test importData() if kornumsatz_origin already exists
    importData(FROM = pathToBackup, TO = TO)
    
    kornInfo <- dbConnect(SQLite(), pathToKornInfo)
    tablesKornInfo <- dbListTables(kornInfo)
    expect_true("kornumsatz_origin" %in% tablesKornInfo)
    
    compareData <- dbReadTable(kornInfo, "kornumsatz_origin")
    dbDisconnect(kornInfo)
    
    expect_equal(
      originalData,
      compareData
    )
  }
)