#' @export

equalise <- function(kornumsatz, product_info, reduce = T, path) {
  # check out if there are any new products
  dif <- setdiff(unique(kornumsatz$Produkt), unique(product_info$Produkte_App))
  
  if (length(dif) != 0) return(dif) # if there is dif return it...
  
  # ... else edit kornumsatz and write it into database
  korn_edit <- foodstorage::startup.settings(kornumsatz, product_info, reduce)
  
  conn <- DBI::dbConnect(SQLite(), path)
  dbWriteTable(
    conn, 
    "kornumsatz_edit",
    korn_edit,
    overwrite = T
  )
  dbDisconnect(conn)
  
  
}