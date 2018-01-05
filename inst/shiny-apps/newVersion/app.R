###################################################################################################
###################### read kornumsatz ############################################################
###################################################################################################
path <<- "/home/simon/Documents/Rprojects"
files <- list.files(file.path(path))
# filter all backups (files which end up with .BAK)
backups <- files[which(stringr::str_detect(files, ".BAK$"))]
current_backup <- backups[length(backups)] # use newest backup

appDB <- dbConnect(SQLite(), file.path(path, current_backup))
kornumsatz_origin <<- dbGetQuery(appDB, '
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
# add 'Bestand.Einheit' column: cumulative sum for every product
kornumsatz_origin <- kornumsatz_origin %>%
  arrange(Produkt) %>%  
  mutate(Bestand.Einheit = round(ave(Menge, Produkt, FUN=cumsum), 3)) %>%
  mutate(Tag = as.Date(Tag, format = "%d/%m/%Y")) %>% 
  arrange(Tag) %>%
  mutate(Tag = as.character(Tag))

### create new data base
mydb <- dbConnect(SQLite(), file.path(path, "kornInfo.sqlite"))

dbWriteTable(
  mydb,
  "kornumsatz_origin",
  kornumsatz_origin,
  overwrite = TRUE
)
# if productInfo already exist, no overwriting won't be necessary
if (!"productInfo" %in% dbListTables(mydb)) {
  data("starting_csv")
  dbWriteTable(
    mydb, 
    "productInfo",
    starting_csv
  )
}

# '<<-' important because kornumsatz must be in globalenv() that it can be found by functions in server UI
kornumsatz <<- dbReadTable(mydb, "kornumsatz_origin") %>%
  mutate(Tag = as.Date(Tag, format = "%Y-%m-%d"))

addProducts <<- checkDifference(kornumsatz,
                       dbReadTable(mydb, "productInfo"))
if (length(addProducts) == 0) {
  equalise(kornumsatz, 
           dbReadTable(mydb, "productInfo"),
           reduce = T,
           pathTOmydb = file.path(path, "kornInfo.sqlite"))
}

dbDisconnect(mydb)


createShinyList <- function(what = "dt", check = FALSE) {
  if (what == "dt") {
    if (check == TRUE) return("plot1")
    plot_output_list <- list(dataTableOutput("plot1"))
  } 
  if (what == "plot") {
    if (check == TRUE) return("plot2")
    plot_output_list <- list(plotOutput("plot2"))
  }
  
  return(plot_output_list)
}

###################################################################################################
########################################### Shiny UI ##############################################
###################################################################################################
ui <- shinyUI(
  navbarPage(
    "Kornkammer", 
    id = "tabs", 
    selected=1,
    #################################### Warenbestand #############################################
    tabPanel(
      "Warenbestand", 
      value = 1, 
      # Header
      h2("Du willst wissen, was in der Kornkammer gerade vorrätig ist?"),
      # select input ...
      fluidRow(
        selectInput(
          "choice",
          "Auswahl",
          choices = list("tabellarisch" = "dt",
                         "grafisch" = "plot")
        )
      ),
      br(),
      fluidRow(
        column(2), # just for a better look
        column(4,
               actionButton("go1", "Nachschlagen")
        )
      ),
      # Main Panel 
      fluidRow(
        uiOutput("plots")
      )
    ),
    
    ############################# new data ########################################################
    tabPanel(
      "Neuen Datensatz abgleichen", 
      value = 2, 
      icon = icon("upload"),
      # Header 
      h2("Hier kannst du einen neuen Datensatz auf Konsistenz überprüfen"),
      h5(em("Falls es seit dem letzten Datensatz neue Produkte gegeben hat, wirst du hier durch eine Benutzeroberfläche geleitet, mittels derer du die neuen Produkte einer Produktgruppe und einem Lieferanten zuordnen kannst.")),
      br(),
      # first row is for name of product in the FoodCoApp and how the product shall be called in future
      fluidRow(
        column(
          4,
          selectizeInput(
            "newproducts", "Produktname in der App",
            choices = c("Bitte wählen" = "",
                        addProducts)
          )
        ),
        column(
          4,
          conditionalPanel(
            condition = "input.newproducts != ''",
            selectizeInput(
              "prodsummary", "Unter folgendem Namem zusammengefasst",
              choices = c("Bitte wählen" = "",
                          "nicht beachten", "neues Produkt")
            ),
            helpText("Wähle 'neues Produkt' wenn wir es wirklich noch nie in der KoKa hatten und nicht in der Liste zu finden ist. Wähle 'nicht beachten', wenn man das Produkt später nicht im Warenbestandsverlauf sehen können soll.")
          )
        ),
        column(
          4,
          conditionalPanel(
            condition = "input.newproducts != '' && input.prodsummary == 'neues Produkt'",
            actionButton("go_prod", "Neues Produkt eintragen", icon = icon("send"))
          )
        )
      ),
      # 2nd row is for deliverers
      fluidRow(
        column(
          4,
          conditionalPanel(
            condition = "input.newproducts != '' && input.prodsummary != '' && input.prodsummary != 'neues Produkt' && input.prodsummary != 'nicht beachten'",
            selectizeInput(
              "deliverer", "Lieferant Nr. 1",
              choices = c("Bitte wählen" = "",
                          levels(starting_csv$Lieferant),
                          "neuer Lieferant")
            ),
            helpText("Wähle 'neuer Lieferant', um einen neuen Lieferanten hinzuzufügen, bei dem zukünftig dieses Produkt bestellt werden soll.")
          )
        ),
        column(
          4,
          conditionalPanel(
            condition = "input.newproducts != '' && input.prodsummary != '' && input.prodsummary != 'neues Produkt' && input.prodsummary != 'nicht beachten'",
            selectizeInput(
              "deliverer2", "Lieferant Nr. 2 (optional)",
              choices = c("Bitte wählen" = "",
                          levels(starting_csv$Lieferant),
                          "neuer Lieferant")
            ),
            helpText("Wähle nichts aus, wenn es keinen zweiten, alternativen Lieferanten für dieses Produkt gibt oder füge einen neuen zweiten Lieferanten für dieses Produkt hinzu.")
          )
        ),
        column(
          4,
          conditionalPanel(
            condition = "input.deliverer == 'neuer Lieferant' || input.deliverer2 == 'neuer Lieferant'",
            actionButton("go_deliv", "neue(n) Lieferanten eintragen", icon = icon("send"))
          )
        )
      ),
      # 3rd row is for product group
      fluidRow(
        column(
          4,
          conditionalPanel(
            condition = "input.newproducts != '' && input.prodsummary != '' && input.prodsummary != 'neues Produkt' && input.prodsummary != 'nicht beachten'",
            selectizeInput(
              "prodgroup", "Produktgruppe",
              choices = c("Bitte wählen" = "",
                          levels(starting_csv$Produktgruppe),
                          "neue Produktgruppe")
            ),
            helpText("Wähle 'neue Produktgruppe', um eine neue Produktgruppe hinzuzufügen.")
          )
        ),
        column(
          4,
          conditionalPanel(
            condition = "input.prodgroup == 'neue Produktgruppe'",
            actionButton("go_group", "neue Produktgruppe hinzufügen", icon = icon("send"))
          )
        )
      ),
      # 4th row is for bulk size
      fluidRow(
        column(
          4,
          conditionalPanel(
            condition = "input.newproducts != '' && input.prodsummary != '' && input.prodsummary != 'neues Produkt' && input.prodsummary != 'nicht beachten'",
            selectInput(
              "bulksize", "Verpackungseinheit",
              choices = c("Bitte wählen" = "",
                          sort(unique(starting_csv$Verpackungseinheit)),
                          "neue VPE")
            ),
            helpText("Wähle 'neue VPE', um eine neue Größe einzugeben.")
          )
        ),
        column(
          4,
          conditionalPanel(
            condition = "input.bulksize == 'neue VPE'",
            actionButton("go_bulk", "neue VPE eintragen", icon = icon("send"))
          )
        )
      ),
      # 5th row is for action button
      fluidRow(
        column(
          4,
          conditionalPanel(
            condition = "input.bulksize != '' || input.prodsummary == 'nicht beachten'",
            actionButton("go2", "Eintragen", icon = icon("send"))
          )
        ),
        column(
          4,
          checkboxInput(
            "options", "Erweiterte Optionen"
          )
        ),
        column(
          4,
          conditionalPanel(
            condition = "input.options == true",
            downloadButton("download", "Starting_CSV exportieren", icon = icon("send"))
          )
        )
      ),
      ############################### create modals #################################################
      bsModal(
        "newprodMOD", "Neuen Produktnamen eintragen",
        trigger = "go_prod",
        textInput("newprod", "zukünftiger Produktname"),
        helpText("Bevor du auf 'eintragen' klickst, vergewissere dich, dass der Name tatsächlich richtig (geschrieben) ist. Danach gibt's kein zurück."),
        actionButton("entry_prod", "Produktname eintragen", icon = icon("send"))
      ),
      bsModal(
        "newdelivMOD", "Neue(n) Lieferanten hinzufügen",
        trigger = "go_deliv",
        conditionalPanel(
          condition = "input.deliverer == 'neuer Lieferant'",
          textInput("newdeliv", "zukünftiger Lieferant Nr.1", value = NULL)
        ),
        conditionalPanel(
          condition = "input.deliverer2 == 'neuer Lieferant'",
          textInput("newdeliv2", "zukünftiger Lieferant Nr. 2 (optional)", value = NULL)
        ),
        helpText("Bevor du auf 'eintragen' klickst, vergewissere dich, dass der Name tatsächlich richtig (geschrieben) ist. Danach gibt's kein zurück."),
        actionButton("entry_deliv", "Lieferanten eintragen", icon = icon("send"))
      ),
      bsModal(
        "newgroupMOD", "Neue Produktgruppe hinzufügen",
        trigger = "go_group",
        textInput("newgroup", "zukünftige Produktgruppe"),
        helpText("Bevor du auf 'eintragen' klickst, vergewissere dich, dass der Name tatsächlich richtig (geschrieben) ist. Danach gibt's kein zurück."),
        actionButton("entry_group", "Produktgruppe eintragen", icon = icon("send"))
      ),
      bsModal(
        "newbulkMOD", "Neue VPE hinzufügen",
        trigger = "go_bulk",
        numericInput("newbulk", "zukünftige VPE", min = 0.1, max = 25, step = 0.1, value = 25),
        helpText("Bevor du auf 'eintragen' klickst, vergewissere dich, dass der Name tatsächlich richtig (geschrieben) ist. Danach gibt's kein zurück."),
        actionButton("entry_bulk", "VPE eintragen", icon = icon("send"))
      )
      
    )            
    # end of tab 'neuen Daten abgleichen'
  ))


###################################################################################################
####################################### Shiny Server ##############################################
###################################################################################################


server <- shinyServer(function(input, output, session){
  
  # first at all: make starting_csv reactive
  rV <- reactiveValues(
    productInfo = dbReadTable(
      dbConnect(SQLite(), file.path(path, "kornInfo.sqlite")),
      "productInfo"
    ),
    addProducts = get("addProducts")
    # print(head(get("productInfo")))
  )
  
  output$plots <- renderUI({
    # Create a list of `plotOutput` objects (depending on current())
    plot_output_list <- createShinyList(input$choice)
    # Place the plot output inside a shiny `tagList()`
    do.call(tagList, plot_output_list)
  })
  
  # Every time a plot changes (button is clicked), re-generate the render functions for all the plots
  observeEvent(
    input$go1,
    label = "renderingPlots", {
      plotname <- createShinyList(input$choice, check = T)
      
      if (plotname == "plot1") {
        output[[plotname]] <- renderDataTable({
          foodstorage::currentStorage(levels(kornumsatz$Produkt), rawlist = TRUE)
        })
      }
      
      if (plotname == "plot2") {
        output[[plotname]] <- renderPlot({
          foodstorage::currentStorage(levels(kornumsatz$Produkt))
        })
      }
    }
  )
  
  #################################################################################################
  ############################ update 'updating productInfo UI' ##################################
  
  # update all inputs after newproducts when some newproducts changes
  observeEvent(input$newproducts, {
    productInfo <- rV$productInfo
    
    # print(str(productInfo))
    updateSelectizeInput(
      session, "prodsummary", "Unter folgendem Namen zusammengefasst",
      choices = c("Bitte wählen" = "",
                  levels(as.factor(productInfo$Produkte_Zusammenfassung)),
                  "neues Produkt", "nicht beachten")
    )
    updateSelectizeInput(
      session, "deliverer", "Lieferant Nr. 1",
      choices = c("Bitte wählen" = "",
                  levels(as.factor(productInfo$Lieferant)),
                  "neuer Lieferant")
    )
    updateSelectizeInput(
      session, "deliverer2", "Lieferant Nr. 2",
      choices = c("Bitte wählen" = "",
                  levels(as.factor(productInfo$Lieferant)))
    )
    updateSelectizeInput(
      session, "prodgroup", "Produktgruppe",
      choices = c("Bitte wählen" = "",
                  levels(as.factor(productInfo$Produktgruppe)),
                  "neue Produktgruppe")
    )
    updateSelectInput(
      session, "bulksize", "Verpackungseinheit",
      choices = c("Bitte wählen" = "",
                  sort(unique(productInfo$Verpackungseinheit)),
                  "neue VPE")
    )
  })
  
  # observe go_... buttons & close bsModals
  observeEvent(input$entry_prod, {
    productInfo <- rV$productInfo
    toggleModal(session, "newprodMOD", toggle = "close")
    updateSelectizeInput(
      session, "prodsummary", "Unter folgendem Namen zusammengefasst",
      choices = sort(c(levels(as.factor(productInfo$Produkte_Zusammenfassung)), 
                       input$newprod)),
      selected = input$newprod
    )
  })
  observeEvent(input$entry_deliv, {
    productInfo <- rV$productInfo
    toggleModal(session, "newdelivMOD", toggle = "close")
    if (!is.null(input$newdeliv)) {
      updateSelectizeInput(
        session, "deliverer", "Lieferant Nr. 1",
        choices = sort(c(levels(as.factor(productInfo$Lieferant)), 
                         input$newdeliv)),
        selected = input$newdeliv
      )
    }
    if (!is.null(input$newdeliv2)) {
      updateSelectizeInput(
        session, "deliverer2", "Lieferant Nr. 2 (optional)",
        choices = sort(c(levels(as.factor(productInfo$Lieferant)), 
                         input$newdeliv2)),
        selected = input$newdeliv2
      )
    }
  })
  observeEvent(input$entry_group, {
    productInfo <- rV$productInfo
    toggleModal(session, "newgroupMOD", toggle = "close")
    updateSelectizeInput(
      session, "prodgroup", "Produktgruppe",
      choices = sort(c(levels(as.factor(productInfo$Produktgruppe)), 
                       input$newgroup)),
      selected = input$newgroup
    )
  })
  observeEvent(input$entry_bulk, {
    productInfo <- rV$productInfo
    toggleModal(session, "newbulkMOD", toggle = "close")
    updateSelectInput(
      session, "bulksize", "Verpackungseinheit",
      choices = sort(c(unique(productInfo$Verpackungseinheit), 
                       input$newbulk)),
      selected = input$newbulk
    )
  })
  
  # eventReactive: get information about deliverers etc. from productInfo, if input$prodsummary 
  # is already known
  prodINFO <- eventReactive(input$prodsummary, {
    if (!input$prodsummary %in% c('neues Produkt', 'nicht beachten', '')) {
      df <- rV$productInfo[rV$productInfo$Produkte_Zusammenfassung == input$prodsummary, ]
      # print(df)
      return(df)
    }
  })
  
  # update of deliverer, deliverer2, prodgroup and bulksize when prodsummary is already known
  observeEvent(prodINFO(), {
    productInfo <- rV$productInfo
    if (!input$prodsummary %in% c('neues Produkt', 'nicht beachten', '')) {
      df <- prodINFO()
      # print(df$Lieferant[1])
      # print(levels(productInfo$Lieferant))
      updateSelectizeInput(
        session, "deliverer", "Lieferant Nr. 1", 
        choices = c(levels(as.factor(productInfo$Lieferant)),
                    "neuer Lieferant"),
        selected = df$Lieferant
      )
      updateSelectizeInput(
        session, "deliverer2", "Lieferant Nr. 2 (optional)",
        choices = levels(as.factor(productInfo$Lieferant)),
        selected = df$Lieferant2
      )
      updateSelectizeInput(
        session, "prodgroup", "Produktgruppe",
        choices = c(levels(as.factor(productInfo$Produktgruppe)),
                    "neue Produktgruppe"),
        selected = df$Produktgruppe
      )
      updateSelectInput(
        session, "bulksize", "Verpackungseinheit",
        choices = c(sort(unique(productInfo$Verpackungseinheit)),
                    "neue VPE"),
        selected = df$Verpackungseinheit
      )
    }
  })
  
  # enter contant in productInfo
  enterContant <- eventReactive(input$go2, {
    # collect information about the product for further analysis
    if (input$prodsummary != 'nicht beachten') {
      newrow <- data.frame(
        "Produkte_App" = input$newproducts, 
        "Produkte_Zusammenfassung" = input$prodsummary,
        "Lieferant" = input$deliverer,
        "Lieferant2" = input$deliverer2,
        "Produktgruppe" = input$prodgroup,
        "Verpackungseinheit" = as.character(input$bulksize)
      )
    }
    # ignore the product for further analysis
    if (input$prodsummary == 'nicht beachten') {
      newrow <- data.frame(
        "Produkte_App" = input$newproducts, 
        "Produkte_Zusammenfassung" = "NI",
        "Lieferant" = "NI",
        "Lieferant2" = "NI",
        "Produktgruppe" = "NI",
        "Verpackungseinheit" = "0"
      )
    }
    return(newrow)
  })
  
  observeEvent(enterContant(), {
    newrow <- enterContant()
    ## for debugging:
    # print(str(productInfo))
    # print(str(newrow))
    # add the new row to productInfo
    rV$productInfo <- rbind(rV$productInfo, newrow)
    # delete row from addProducts where ProdukteApp == input$newproducts
    rV$addProducts <<- rV$addProducts[which(rV$addProducts != input$newproducts)]
    
    # make entry in data base
    kornInfo <- src_sqlite(file.path(path, "kornInfo.sqlite"))
    db_insert_into(kornInfo$con, "productInfo", newrow)
    # dbReadTable(kornInfo$con, "productInfo") %>%
    #   tail(5) %>% print()
    
    # update selectizeInput of newproducts
    updateSelectizeInput(
      session, "newproducts", "Produktname in der App",
      choices = c("Bitte wählen" = "",
                  rV$addProducts)
    )
  })
  
  # render DownloadButton: export productInfo
  output$download <- downloadHandler(
      filename = function() {
        paste('productInfo-', Sys.Date(), '.csv', sep='')
      },
      content = function(file) {
        # print(rV$productInfo)
        write.table(rV$productInfo, file, sep = ";", row.names = F)
      }
    )
})

####################################### shiny app executing ################################################
shinyApp(ui, server, options = list(port = 1234))