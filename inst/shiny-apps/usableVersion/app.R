###############################################################################
###################### read kornumsatz ########################################
###############################################################################
path <- "/home/simon/Documents/Rprojects/unknownProducts/"
files <- list.files(file.path(path))
# filter all backups (files which end up with .BAK)
backups <- files[which(stringr::str_detect(files, ".BAK$"))]
current_backup <- backups[length(backups)] # use newest backup

pathToBackup <- file.path(path, current_backup)

kornInfo <- files[which(stringr::str_detect(files, "kornInfo.sqlite"))]
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

### set filter options
filter <- c(
  "alles" = "everything",
  "nur verfügbare Lebensmittel" = "available",
  "alle leeren LM" = "empty"
)

###############################################################################
########################### Shiny UI ##########################################
###############################################################################
ui <- shinyUI(
  navbarPage(
    "Kornkammer", 
    id = "tabs", 
    selected = 1,
    ####################### Warenbestand ######################################
    tabPanel(
      "Warenbestand", 
      value = 1,
      icon = icon("table"),
      # Header
      h2("Du willst wissen, was in der Kornkammer gerade vorrätig ist?"),
      br(), # empty row
      fluidRow(
        uiOutput("filter")
      ),
      br(),
      # dataTable = mainPanel
      fluidRow(
        column(12, DT::dataTableOutput("storage"))
      )
    ), # end of tabPanel == 1
    
    tabPanel(
      "Produktinformationen",
      value = 2,
      icon = icon("tasks"),
      h2("Hier findet ihr alle Produktinformationen zum Bearbeiten"),
      br(), # empty row
      
      fluidRow(column(12, uiOutput("editProductInfo"))),
      
      fluidRow(
        column(12, DT::dataTableOutput("productInfo"))
      )
    )
  ) # end of navbarPage
) # end of shinyUI

###############################################################################
############################ Shiny Server #####################################
###############################################################################


server <- shinyServer(function(input, output, session){
  #############################################################################
  ######################## reactive part for all tab panels ###################
  # first of all: make starting_csv reactive
  rV <- reactiveValues(
    productInfo = matrix(c(1:10), nrow = 2), # example data
    addProducts = c("example")
  )
  
  # which data are currently requested from the user?
  currentData <- eventReactive(input$tabs, {
    #### load data from kornInfo.sqlite ####
    kornInfo <- DBI::dbConnect(RSQLite::SQLite(), pathToKornInfo)
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
    
    # if there is no difference, data are up to date...
    if (length(dif) == 0) {
      #... which means that we can add product information to food storage data
      editData <- startupSettings(originalData, productInfo)
      
      # write edited data into database
      DBI::dbWriteTable(
        kornInfo,
        "kornumsatz_edit",
        editData,
        overwrite = T
      )
      DBI::dbDisconnect(kornInfo)
      
      return(list(
        editData = editData,
        productInfo = productInfo,
        dataAreUpToDate = TRUE
      ))
      
    } else {
      DBI::dbDisconnect(kornInfo)
      
      difDF <- tibble(
        Produkte_App = dif,
        Produkte_Zusammenfassung = rep("", length(dif)),
        Lieferant = rep("", length(dif)),
        Lieferant2 = rep("", length(dif)),
        Produktgruppe = rep("", length(dif)),
        Verpackungseinheit = rep(NA, length(dif))
      )
      
      # add products to productInfo which have no information yet
      productInfo <- dplyr::arrange(
        dplyr::bind_rows(productInfo, difDF), Produkte_Zusammenfassung
      )
      return(list(
        originalData = originalData,
        productInfo = productInfo,
        # addProducts = dif,
        # newProducts = newProducts,
        dataAreUpToDate = FALSE
      ))
    }
  })
  
  # render 'filter'-UI when dataset is up to date
  output$filter <- renderUI({
    if (currentData()$dataAreUpToDate == TRUE) {
      selectInput(
        "filter",
        "Welche Produkte sollen angezeigt werden?",
        choices = filter
      )
    }
  })
  
  # update reactive values
  observeEvent(currentData(), {
    if (currentData()$dataAreUpToDate == FALSE) {
      rV$productInfo <- currentData()$productInfo
      rV$addProducts <- currentData()$addProducts
    }
  })
  
  
  #############################################################################
  ############# reactive part specially for foodstorage panel #################
  output$storage <- DT::renderDataTable({
    if (currentData()$dataAreUpToDate == TRUE) {
      # show a datatable including product infos
      return(prepareDatatable(
        currentData()$editData,
        filter = input$filter
      ))
    }
    if (currentData()$dataAreUpToDate == FALSE) {
      # show a datatable with the current food storage without product infos
      return(currentData()$originalData)
    }
  })
  
  #############################################################################
  ################## reactive part specially for product information ##########
  # output$productInfoInput <- renderUI({
  #   if (length(currentData()) == 4) {
  #     checkboxGroupInput(
  #       "what", "Alle oder nur fehlende Produkte anzeigen?", 
  #       choices = c("alle", "nur fehlende Produkte"), selected = "alle"
  #     )
  #   }
  # })
  
  
  output$productInfo <- DT::renderDataTable({
    data <- currentData()$productInfo
    return(DT::datatable(data, selection = "single"))
  })
  
  # no we wanna create the UIoutput to edit product information
  output$editProductInfo <- renderUI({
    # only render when a row is selected (dont render when currentData changes)
    if (!is.null(input$productInfo_rows_selected)) {
      currentRow <- dplyr::slice(
        isolate(currentData()$productInfo), input$productInfo_rows_selected
      )
      
      prodInfo <- isolate(currentData()$productInfo)
      tagList(
        fluidRow(
          column(
            3,
            selectizeInput(
              "productSummary", "Produkte Zusammenfassung",
              choices = unique(prodInfo$Produkte_Zusammenfassung),
              selected = currentRow$Produkte_Zusammenfassung,
              options = list(create = TRUE)
            )
          ),
          column(
            2,
            selectizeInput(
              "deliverer1", "Lieferant Nr.1",
              choices = unique(prodInfo$Lieferant),
              selected = currentRow$Lieferant,
              options = list(create = TRUE)
            )
          ),
          column(
            2,
            selectizeInput(
              "deliverer2", "Lieferant Nr.2",
              choices = unique(prodInfo$Lieferant2),
              selected = currentRow$Lieferant2,
              options = list(create = TRUE)
            )
          ),
          column(
            3,
            selectizeInput(
              "productGroup", "Produktgruppe",
              choices = unique(prodInfo$Produktgruppe),
              selected = currentRow$Produktgruppe,
              options = list(create = TRUE)
            )
          ),
          column(
            2,
            selectizeInput(
              "bulksize", "VPE",
              choices = unique(prodInfo$Verpackungseinheit),
              selected = currentRow$Verpackungseinheit,
              options = list(create = TRUE)
            )
          )
        )
      )
    }
  })
  
}) # end of server part

###################### execute shiny app ######################################
shinyApp(ui, server, options = list(port = 1234))