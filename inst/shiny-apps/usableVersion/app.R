library(shiny)

###############################################################################
###################### read kornumsatz ########################################
###############################################################################
### define path where shiny app shall look for backup and kornInfo
path <- "/home/shiny"
stopifnot( "backup" %in% dir(file.path(path)) )
pathToBackupDir <- file.path(path, "backup")
backup <- list.files(pathToBackupDir, pattern = "\\.BAK$")
# ensure that we have only one backup in our directory
stopifnot( length(backup) == 1 )
# define path to backup
pathToBackup <- file.path(pathToBackupDir, backup)

# do the same for kornInfo
stopifnot( "kornInfo" %in% dir(file.path(path)) )
pathToKorninfoDir <- file.path(path, "kornInfo")
kornInfo <- list.files(pathToKorninfoDir, pattern = "\\.sqlite$")
stopifnot( length(kornInfo) == 1 )
# <<- necessary because its reactive in the server part
pathToKornInfo <<- file.path(pathToKorninfoDir, kornInfo) 

secondDatabase <- list(
  nameTable = "kornumsatz_origin",
  path = pathToKornInfo
)

foodstorage::importData(
  FROM = pathToBackup,
  TO = secondDatabase
)

### set filter options
filter <- c(
  "alles" = "everything",
  "nur verfügbare Lebensmittel" = "available",
  "alle leeren LM" = "empty"
)

saveData <- function(data) {
  # Connect to the database
  db <- DBI::dbConnect(RSQLite::SQLite(), pathToKornInfo)
  
  prodInfo <- DBI::dbReadTable(db, "productInfo")
  requestedRow <- dplyr::filter(
    prodInfo,
    Produkte_App == data[1, "Produkte_App"]
  )
  
  if (nrow(requestedRow) != 0) {
    # Construct the update query by looping over the data fields
    query <- sprintf(
      "UPDATE %s SET %s WHERE Produkte_App = %s",
      "productInfo", 
      paste0(names(data), " = '", data, "'", collapse = ", "),
      paste0("'", data[1, "Produkte_App"], "'")
    )
  } else {
    query <- sprintf(
      "INSERT INTO %s (%s) VALUES ('%s')",
      "productInfo", 
      paste(names(data), collapse = ", "),
      paste(data, collapse = "', '")
    )
  }
  
  # Submit the update query and disconnect
  DBI::dbGetQuery(db, query)
  
  # for debugging
  # prodInfo <- dbReadTable(db, "productInfo")
  # print(prodInfo[which(prodInfo$Produkte_App == data[1, "Produkte_App"]), ])
  DBI::dbDisconnect(db)
}

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
      
      fluidRow(
        column(12, uiOutput("editProductInfo"))
      ),
      
      fluidRow(
        column(2, offset = 5, uiOutput("saveButton"))
      ),
      
      fluidRow(
        column(12, DT::dataTableOutput("displayProductInfo"))
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
  #### first of all: safe productInfo as a reactiveValue ####
  # ones it gets feeded with data by currentData() (which means by changing the
  # tab), it gets updated everytime the saveButton is pressed
  reactiveData <- reactiveValues(
    productInfo = NULL
  )
  
  # currentData() returns different data depending on whether data are up to 
  # date or not
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
    dif <- foodstorage::checkDifference(originalData, productInfo)
    
    # if there is no difference, data are up to date...
    if (length(dif) == 0) {
      #... which means that we can add product information to food storage data
      edittedData <- foodstorage::startupSettings(originalData, productInfo)
      
      # write edited data into database
      DBI::dbWriteTable(
        kornInfo,
        "kornumsatz_edit",
        edittedData,
        overwrite = T
      )
      DBI::dbDisconnect(kornInfo)
      
      return(list(
        edittedData = edittedData,
        productInfo = productInfo,
        dataAreUpToDate = TRUE
      ))
      
    } else {
      DBI::dbDisconnect(kornInfo)
      
      difDF <- dplyr::tibble(
        Produkte_App = dif,
        Produkte_Zusammenfassung = "",
        Lieferant = "",
        Lieferant2 = "",
        Produktgruppe = "",
        Verpackungseinheit = NA
      )
      
      # add products to productInfo which have no information yet
      productInfo <- dplyr::arrange(
        dplyr::bind_rows(productInfo, difDF), 
        Produkte_Zusammenfassung
      )
      return(list(
        originalData = originalData,
        productInfo = productInfo,
        dataAreUpToDate = FALSE
      ))
    }
  })
  
  # update productInfo as an reactive value everytime currentData() changes 
  observeEvent(currentData(), {
    # productInfo will always be returned
    reactiveData$productInfo <- currentData()$productInfo    
  })
  
  #############################################################################
  ############# reactive part specially for foodstorage panel #################
  # render 'filter'-UI when dataset is up to date
  output$filter <- renderUI({
    if (currentData()$dataAreUpToDate == TRUE) {
      selectInput(
        "filter",
        "Welche Produkte sollen angezeigt werden?",
        choices = filter # possibilities are defined above
      )
    }
  })
  
  # whether data are up to date or not, main plot visualizes different data
  output$storage <- DT::renderDataTable({
    if (currentData()$dataAreUpToDate == TRUE) {
      # show a datatable including product infos
      if (!is.null(input$filter)) {
        return(foodstorage::prepareDatatable(
          currentData()$edittedData,
          filter = input$filter
        ))
      }
    }
    if (currentData()$dataAreUpToDate == FALSE) {
      # show a datatable with the current food storage without product infos
      currentStorage <- foodstorage::currentStorage(
        dataset = currentData()$originalData,
        dataAreUpToDate = FALSE
      )
      return(currentStorage)
    }
  })
  
  #############################################################################
  ################## reactive part specially for product information ##########
  # at first render productInfo datatable
  output$displayProductInfo <- DT::renderDataTable({
    data <- reactiveData$productInfo
    return(DT::datatable(data, selection = "single"))
  })
  
  # no we wanna create the UIoutput to edit product information
  output$editProductInfo <- renderUI({
    # only render when a row is selected (dont render when currentData changes)
    if (!is.null(input$displayProductInfo_rows_selected)) {
      isolate({
        prodInfo <- reactiveData$productInfo
        # selectedRowIndex <- input$displayProductInfo_rows_selected
        selectedRow <- dplyr::slice(
          prodInfo,
          input$displayProductInfo_rows_selected
        )
        
        tagList(
          fluidRow(
            column(
              3, 
              selectizeInput(
                "productSummary", "Produkte Zusammenfassung",
                choices = unique(prodInfo$Produkte_Zusammenfassung),
                selected = selectedRow$Produkte_Zusammenfassung,
                options = list(create = TRUE)
              )
            ),
            column(
              2,
              selectizeInput(
                "deliverer1", "Lieferant Nr.1",
                choices = unique(prodInfo$Lieferant),
                selected = selectedRow$Lieferant,
                options = list(create = TRUE)
              )
            ),
            column(
              2,
              selectizeInput(
                "deliverer2", "Lieferant Nr.2",
                choices = unique(prodInfo$Lieferant2),
                selected = selectedRow$Lieferant2,
                options = list(create = TRUE)
              )
            ),
            column(
              3,
              selectizeInput(
                "productGroup", "Produktgruppe",
                choices = unique(prodInfo$Produktgruppe),
                selected = selectedRow$Produktgruppe,
                options = list(create = TRUE)
              )
            ),
            column(
              2,
              selectizeInput(
                "bulksize", "VPE",
                choices = unique(prodInfo$Verpackungseinheit),
                selected = selectedRow$Verpackungseinheit,
                options = list(create = TRUE)
              )
            )
          ), # end of first fluidRow
          fluidRow(
            column(
              2, offset = 1,
              actionButton(
                "saveButton", "Änderungen speichern", icon = icon("save")
              )
            )
          ) # end of second fluidRow
        ) # end of tagList
      }) # end of isolate()
    } # end of if condition (if a row is selected)
    
  }) # end of renderUI of editProductInf
  
  # observe product summary: if the chosen name is already known, use autofill
  # to fill the other columns
  observeEvent(input$productSummary, {
    prodInfo <- isolate(reactiveData$productInfo)
    # if chosen product summary is already known, all the other inputs will be
    # updated
    if (input$productSummary %in% unique(prodInfo$Produkte_Zusammenfassung)) {
      # filter prodInfo by the product summary which was chosen by the user
      autofillRow <- dplyr::filter(
        prodInfo,
        Produkte_Zusammenfassung == input$productSummary
      )
      # update the adequate inputs...
      updateSelectizeInput(
        session, "deliverer1", 
        selected = dplyr::pull(autofillRow, Lieferant)[1]
      )
      updateSelectizeInput(
        session, "deliverer2",
        selected = dplyr::pull(autofillRow, Lieferant2)[1]
      )
      updateSelectizeInput(
        session, "productGroup",
        selected = dplyr::pull(autofillRow, Produktgruppe)[1]
      )
      updateSelectInput(
        session, "bulksize",
        selected = dplyr::pull(autofillRow, Verpackungseinheit)[1]
      )
    }
  })
  
  # observe save button and if it's pressed, data will be saved into database
  # and reactiveData will be updated
  observeEvent(input$saveButton, {
    
    # load productInfo to get the original name of the requested product
    prodInfo <- reactiveData$productInfo
    selectedRow <- input$displayProductInfo_rows_selected
    requestedProduct <- prodInfo$Produkte_App[selectedRow]
    
    # everytime save button is pressed, update current row
    # get new values from the inputs
    updatedRow <- data.frame(
      Produkte_App = requestedProduct,
      Produkte_Zusammenfassung = input$productSummary,
      Lieferant = input$deliverer1,
      Lieferant2 = input$deliverer2,
      Produktgruppe = input$productGroup,
      Verpackungseinheit = input$bulksize,
      stringsAsFactors = FALSE
    )
    
    # update current row of productInfo except product's name in the app 
    # (= "Produkte_App")
    reactiveData$productInfo[selectedRow, ] <- updatedRow[1, ]
    
    # save data into data base
    saveData(updatedRow)
  })
}) # end of server part

###################### execute shiny app ######################################
shinyApp(ui, server)
