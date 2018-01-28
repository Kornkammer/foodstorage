###################################################################################################
###################### read kornumsatz ############################################################
###################################################################################################
path <- "/home/simon/Documents/Rprojects"
files <- list.files(file.path(path))
# filter all backups (files which end up with .BAK)
backups <- files[which(stringr::str_detect(files, ".BAK$"))]
current_backup <- backups[length(backups)] # use newest backup

pathToBackup <- file.path(path, current_backup)

kornInfo <- files[which(stringr::str_detect(files, "kornInfo.sqlite"))]
print(length(kornInfo))
stopifnot(length(kornInfo) == 1)
# <<- necessary because its reactive in the server part
pathToKornInfo <<- file.path(path, kornInfo) 


secondDatabase <- list(
  nameTable = "kornumsatz_origin",
  path = pathToKornInfo
)

importData(
  FROM = pathToBackup,
  TO = secondDatabase
)
# createShinyList <- function(what = "dt", check = FALSE) {
#   if (what == "dt") {
#     if (check == TRUE) return("plot1")
#     plot_output_list <- list(dataTableOutput("plot1"))
#   } 
#   if (what == "plot") {
#     if (check == TRUE) return("plot2")
#     plot_output_list <- list(plotOutput("plot2"))
#   }
#   
#   return(plot_output_list)
# }

###############################################################################
########################### Shiny UI ##########################################
###############################################################################
ui <- shinyUI(
  navbarPage(
    "Kornkammer", 
    id = "tabs", 
    selected=1,
    ####################### Warenbestand ######################################
    tabPanel(
      "Warenbestand", 
      value = 1,
      icon = icon("table"),
      # Header
      h2("Du willst wissen, was in der Kornkammer gerade vorrätig ist?"),
      # select input ...
      # fluidRow(
      #   selectInput(
      #     "choice",
      #     "Auswahl",
      #     choices = list("tabellarisch" = "dt",
      #                    "grafisch" = "plot")
      #   )
      # ),
      # br(),
      # fluidRow(
      #   column(2), # just for a better look
      #   column(4,
      #          actionButton("go1", "Nachschlagen")
      #   )
      # ),
      # Main Panel 
      fluidRow(
        dataTableOutput("storage")
      )
    ),
    ############################# new data ####################################
    tabPanel(
      "Neuen Datensatz abgleichen", 
      value = 2, 
      icon = icon("tasks"),
      # Header 
      h2("Hier kannst du einen neuen Datensatz auf Konsistenz überprüfen"),
      h5(em("Falls es seit dem letzten Datensatz neue Produkte gegeben hat, ",
            "wirst du hier durch eine Benutzeroberfläche geleitet, mittels ",
            "derer du die neuen Produkte einer Produktgruppe und einem ",
            "Lieferanten zuordnen kannst.")),
      br(),
      # first row is for name of a product in the FoodCoApp and how the product
      # shall be called in future
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
            helpText(
              "Wähle 'neues Produkt' wenn wir es wirklich noch nie in der ",
              "Koka hatten und nicht in der Liste zu finden ist. Wähle 'nicht ",
              "beachten', wenn man das Produkt später nicht im ",
              "Warenbestandsverlauf sehen können soll."
            )
          )
        ),
        column(
          4,
          conditionalPanel(
            condition = "input.newproducts != '' && 
            input.prodsummary == 'neues Produkt'",
            actionButton(
              "go_prod", "Neues Produkt eintragen", icon = icon("send")
            )
          )
        )
      ),
      # 2nd row is for deliverers
      fluidRow(
        column(
          4,
          conditionalPanel(
            condition = "input.newproducts != '' && input.prodsummary != '' && 
            input.prodsummary != 'neues Produkt' && 
            input.prodsummary != 'nicht beachten'",
            selectizeInput(
              "deliverer", "Lieferant Nr. 1",
              choices = c("Bitte wählen" = "",
                          levels(starting_csv$Lieferant),
                          "neuer Lieferant")
            ),
            helpText(
              "Wähle 'neuer Lieferant', um einen neuen Lieferanten hinzuzufügen,",
              "bei dem zukünftig dieses Produkt bestellt werden soll."
            )
          )
        ),
        column(
          4,
          conditionalPanel(
            condition = "input.newproducts != '' && input.prodsummary != '' && 
            input.prodsummary != 'neues Produkt' && 
            input.prodsummary != 'nicht beachten'",
            selectizeInput(
              "deliverer2", "Lieferant Nr. 2 (optional)",
              choices = c("Bitte wählen" = "",
                          levels(starting_csv$Lieferant),
                          "neuer Lieferant")
            ),
            helpText(
              "Wähle nichts aus, wenn es keinen zweiten, alternativen Lieferanten",
              "für dieses Produkt gibt oder füge einen neuen zweiten Lieferanten",
              "für dieses Produkt hinzu."
            )
          )
        ),
        column(
          4,
          conditionalPanel(
            condition = "input.deliverer == 'neuer Lieferant' || 
            input.deliverer2 == 'neuer Lieferant'",
            actionButton(
              "go_deliv", "neue(n) Lieferanten eintragen", icon = icon("send")
            )
          )
        )
      ),
      # 3rd row is for product group
      fluidRow(
        column(
          4,
          conditionalPanel(
            condition = "input.newproducts != '' && input.prodsummary != '' && 
            input.prodsummary != 'neues Produkt' && 
            input.prodsummary != 'nicht beachten'",
            selectizeInput(
              "prodgroup", "Produktgruppe",
              choices = c("Bitte wählen" = "",
                          levels(starting_csv$Produktgruppe),
                          "neue Produktgruppe")
            ),
            helpText(
              "Wähle 'neue Produktgruppe', um eine neue Produktgruppe ",
              "hinzuzufügen."
            )
          )
        ),
        column(
          4,
          conditionalPanel(
            condition = "input.prodgroup == 'neue Produktgruppe'",
            actionButton(
              "go_group", "neue Produktgruppe hinzufügen", icon = icon("send")
            )
          )
        )
      ),
      # 4th row is for bulk size
      fluidRow(
        column(
          4,
          conditionalPanel(
            condition = "input.newproducts != '' && input.prodsummary != '' && 
            input.prodsummary != 'neues Produkt' && 
            input.prodsummary != 'nicht beachten'",
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
            condition = "input.bulksize != '' || 
            input.prodsummary == 'nicht beachten'",
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
            downloadButton(
              "download", "Starting_CSV exportieren", icon = icon("send")
            )
          )
        )
      ),
      ############################### create modals ###########################
      bsModal(
        "newprodMOD", "Neuen Produktnamen eintragen",
        trigger = "go_prod",
        textInput("newprod", "zukünftiger Produktname"),
        helpText(
          "Bevor du auf 'eintragen' klickst, vergewissere dich, dass der Name",
          "tatsächlich richtig (geschrieben) ist. Danach gibt's kein zurück."
        ),
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
          textInput(
            "newdeliv2", "zukünftiger Lieferant Nr. 2 (optional)", value = NULL
          )
        ),
        helpText(
          "Bevor du auf 'eintragen' klickst, vergewissere dich, dass der Name",
          " tatsächlich richtig (geschrieben) ist. Danach gibt's kein zurück."
        ),
        actionButton("entry_deliv", "Lieferanten eintragen", icon = icon("send"))
      ),
      bsModal(
        "newgroupMOD", "Neue Produktgruppe hinzufügen",
        trigger = "go_group",
        textInput("newgroup", "zukünftige Produktgruppe"),
        helpText(
          "Bevor du auf 'eintragen' klickst, vergewissere dich, dass der Name",
          " tatsächlich richtig (geschrieben) ist. Danach gibt's kein zurück."
        ),
        actionButton("entry_group", "Produktgruppe eintragen", icon = icon("send"))
      ),
      bsModal(
        "newbulkMOD", "Neue VPE hinzufügen",
        trigger = "go_bulk",
        numericInput("newbulk", "zukünftige VPE", min = 0.1, max = 25, step = 0.1, value = 25),
        helpText(
          "Bevor du auf 'eintragen' klickst, vergewissere dich, dass der Name",
          " tatsächlich richtig (geschrieben) ist. Danach gibt's kein zurück."
        ),
        actionButton("entry_bulk", "VPE eintragen", icon = icon("send"))
      )
      
    )            
    # end of tab 'neuen Daten abgleichen'
  ))


###############################################################################
############################ Shiny Server #####################################
###############################################################################


server <- shinyServer(function(input, output, session){
  
  # first of all: make starting_csv reactive
  rV <- reactiveValues(
    productInfo = datatable(matrix(c(1:10), nrow = 2)), # example data
    addProducts = c("example")
    # print(head(get("productInfo")))
  )
  
  currentData <- eventReactive(tabs, {
    #### load data from kornInfo.sqlite ####
    kornInfo <- DBI::dbConnect(RSQlite::SQLite(), pathToKornInfo)
    originalData <- DBI::dbReadTable(
      kornInfo,
      "kornumsatz_origin"
    )
    productInfo <- DBI::dbReadTable(
      kornInfo,
      "productInfo"
    )
    
    #### check difference between original data and product information ####
    dif <- checkDifference(originalData, productInfo)
    
    # if there is no difference, data are up to date
    if (length(dif) == 0) {
      editData <- startupSettings(originalData, productInfo)
      
      # write edited data into database
      DBI::dbWriteTable(
        kornInfo,
        "kornumsatz_edit",
        editData,
        overwrite = T
      )
      DBI::dbDisconnect(kornInfo)
      
      return(editData)
      
    } else {
      return(list(
        originalData = originalData,
        productInfo = datatable(productInfo),
        addProducts = dif
      ))
    }
  })
  
  
  output$storage <- renderDataTable({
    if (is.data.frame(currentData())) {
      # show a datatable including product infos
      datatable(currentData())
    }
    if (is.list(currentData())) {
      # show a datatable with the current food storage without product infos
      data <- currentData()$originalData
      datatable(data)
    }
  })
  
  observeEvent(currentData(), {
    if (is.list(currentData())) {
      rV$productInfo <- currentData()$productInfo
      rV$addProducts <- currentData()$addProducts
    }
  })
  
  
  #############################################################################
  ################## update 'updating productInfo UI' #########################
  
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