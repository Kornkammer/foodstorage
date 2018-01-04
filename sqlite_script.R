# start trial: use RSQLITE
library(DBI)
orig <- dbConnect(SQLite(), "data/Backup_2017_09_19_origin.BAK")
dbListTables(orig)

appDB <- dbConnect(SQLite(), paste0("data/", list.files("data")[1])) # connect to trial backup
kornumsatz_origin <- dbGetQuery(appDB, '
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
# dbDisconnect(mydb)

# add 'Bestand.Einheit' column: cumulative sum for every product
kornumsatz_origin <- kornumsatz_origin %>%
  arrange(Produkt) %>%  
  mutate(Bestand.Einheit = round(ave(Menge, Produkt, FUN=cumsum), 3)) %>%
  mutate(Tag = as.Date(Tag, format = "%d/%m/%Y")) %>% 
  arrange(Tag) %>%
  mutate(Tag = as.character(Tag))

### create new data base
mydb <- dbConnect(SQLite(), "data/kornInfo.sqlite")

dbWriteTable(
  mydb,
  "kornumsatz_origin",
  kornumsatz_origin,
  overwrite = T
)

dbWriteTable(
  mydb, 
  "product_info",
  read.csv("/home/simon/Documents/Studium/Bachelor-Arbeit/R-paket/foodstorage/data/starting_csv_20171125.csv")[,-1],
  overwrite = TRUE
)
dbListTables(mydb)
library(foodstorage)

kornumsatz_origin <- dbReadTable(mydb, "kornumsatz_origin") %>%
  mutate(Tag = as.Date(Tag, format = "%Y-%m-%d"))

kornumsatz_edit <- foodstorage::startup.settings(
  kornumsatz_origin,
  dbReadTable(mydb, "product_info"), 
  reduce = T
)

dbWriteTable(
  mydb,
  "kornumsatz_edit",
  kornumsatz_edit,
  overwrite = T
)

# test
library(testthat)
test_that(
  "cumulative sum for every product works", {
    dat <- dbReadTable(mydb, "kornumsatz_origin") %>%
      filter(Produkt == "Olivenöl") %>%
      mutate(cumsum = cumsum(Menge)) %>%
      mutate(Tag = as.Date(Tag, format = "%Y-%m-%d")) %>%
      select(Tag, Bestand.Einheit, cumsum)
    
    expect_equal(
      round(dat[,2] - dat[,3], 3),
      rep(0,nrow(dat))
    )
  }
)


### bank transactions
bank <- dbGetQuery(orig, "
  SELECT quantity, 
        strftime(\'%d/%m/%Y\',start/1000,\'unixepoch\') AS Tag, 
        comment
  FROM transaction_products
  JOIN transactions ON transaction_products.transaction_id = transactions._id
  WHERE account_guid IS 'bank'
") %>%  
  mutate(Tag = as.Date(Tag, format = "%d/%m/%Y")) %>% 
  arrange(Tag)

bank2016 <- bank %>%
  filter(year(Tag) == 2016)

bank <- dbGetQuery(orig, "
  SELECT * 
  FROM transaction_products
  WHERE account_guid IS 'bank'
")
