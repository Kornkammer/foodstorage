# start trial: use RSQLITE
library(DBI)
mydb <- dbConnect(SQLite(), "data/Knk_2017_05_12--13:12.BAK")
kornumsatz_origin <- dbGetQuery(mydb, '
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
dbDisconnect(mydb)
# add 'Bestand.Einheit' column: cumulative sum for every product
kornumsatz_origin <- kornumsatz_origin %>%
  # transform Tag to class Date
  mutate(Tag = as.Date(Tag, format = "%d/%m/%Y")) %>% 
  # sort by product
  arrange(Produkt) %>%  
  # add cumulative sum for every product...
  mutate(Bestand.Einheit = ave(Menge, Produkt, FUN=cumsum)) %>% 
  # ... and sort by day again
  arrange(Tag) %>%
  select(Bestand.Einheit)
dbWriteTable(
  mydb,
  "kornumsatz_origin",
  kornumsatz_origin,
  overwrite = T,
  append = T
)
dbWriteTable(
  mydb, 
  "product_info",
  read.csv("/home/simon/Documents/Studium/Bachelor-Arbeit/R-paket/foodstorage/data/starting_csv_20171125.csv")[,-1],
  overwrite = TRUE
)
dbListTables(mydb)
library(foodstorage)

kornumsatz_edit <- foodstorage::startup.settings(
  dbReadTable(mydb, "kornumsatz_origin"), 
  dbReadTable(mydb, "product_info"), reduce = T
)

print(head(dbReadTable(mydb, "kornumsatz_origin")))

# test
p2 <- kornumsatz_origin$Bestand.Einheit[which(kornumsatz_origin$Produkt == "Olivenöl")]
p1 <- cumsum(kornumsatz_origin$Menge[which(kornumsatz_origin$Produkt == "Olivenöl")])
p2-p1 # Test ob es funktioniert -> ist gleich
plot(p1 ~ kornumsatz_origin$Tag[which(kornumsatz_origin$Produkt == "Olivenöl")], type = "l") 
plot(p2)

