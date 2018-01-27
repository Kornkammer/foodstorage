#' @title import data from Food Co-op's Backup
#' @description \code{importData()} connects to a Food Co-op's SQLite database.
#' A query gives back the current turnover of the Food Co-op and writes the 
#' result in a second database. 
#' @param FROM the path to the backup database.
#' @param TO a list containing 'nameTable' and 'path' of the second
#' database = the database where the query shall be dropped
#' @details 
#' The names of the character strings inside the list of parameter \code{TO}
#' need to be the same as they are writte above (or look at examples).
#' @return 
#' No value is returned but a table is written in the second database.
#' @examples 
#' #### load dataset ####
#' path <- system.file("data", package = "foodstorage")
#' files <- list.files(path)
#' # filter all backups (files which end up with .BAK)
#' backups <- files[which(stringr::str_detect(files, ".BAK$"))]
#' current_backup <- backups[length(backups)] # use newest backup
#' pathToBackup <- file.path(path, current_backup)
#' 
#' # test result of file.path -> if file doesn't exist, length(result) will be 0
#' kornInfoExists <- files[which(stringr::str_detect(files, "kornInfo.sqlite"))]
#' pathToKornInfo <- file.path(path, kornInfoExists)
#' # create list for parameter 'TO'
#' TO <- list(nameDB = "kornInfo.sqlite", nameTable = "kornumsatz_origin", path = pathToKornInfo)
#' 
#' importData(FROM = pathToBackup, TO = TO)
#' @export

importData <- function(FROM, TO) {
  
  backup <- DBI::dbConnect(RSQLite::SQLite(), FROM)
  originalData <- DBI::dbGetQuery(backup, '
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
  DBI::dbDisconnect(backup)
  
  ### create new data base
  mydb <- DBI::dbConnect(RSQLite::SQLite(), TO$path)
  
  DBI::dbWriteTable(
    mydb,
    TO$nameTable,
    originalData,
    overwrite = TRUE
  )
  DBI::dbDisconnect(mydb)
}