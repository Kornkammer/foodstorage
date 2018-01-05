#' @title generates an edited dataset
#' @description If \code{checkDifference} doesn't recognize any differences (anymore),
#' this function will generate an edited dataset, usually kornumsatz.
#' @export

equalise <- function(dataset, productInfo, reduce = T, pathTOmydb) {

  # ... else edit kornumsatz and write it into database
  kornumsatz_edit <- foodstorage::startup.settings(
    dataset,
    dbReadTable(mydb, productInfo), 
    reduce = T
  )
  
  conn <- DBI::dbConnect(SQLite(), pathTOmydb)
  dbWriteTable(
    conn, 
    "kornumsatz_edit",
    kornumsatz_edit,
    overwrite = T
  )
  dbDisconnect(conn)
}