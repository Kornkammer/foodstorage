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
      
      fluidRow(
        column(3, uiOutput("editProductSummary")),
        column(9, uiOutput("editAnyOtherInputColumns"))
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
  #### first of all: make starting_csv reactive ####
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
    dif <- checkDifference(originalData, productInfo)
    
    # if there is no difference, data are up to date...
    if (length(dif) == 0) {
      #... which means that we can add product information to food storage data
      edittedData <- startupSettings(originalData, productInfo)
      
      # write edited data into database
      DBI::dbWriteTable(
        kornInfo,
        "kornumsatz_edit",
        editData,
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
        choices = filter
      )
    }
  })
  
  # whether data are up to date or not, main plot visualizes different data
  output$storage <- DT::renderDataTable({
    if (currentData()$dataAreUpToDate == TRUE) {
      # show a datatable including product infos
      return(prepareDatatable(
        currentData()$edittedData,
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
  # at first render productInfo datatable
  output$displayProductInfo <- DT::renderDataTable({
    data <- reactiveData$productInfo
    return(DT::datatable(data))
  })
  
  # everytime a row gets clicked, this function returns which ones are selected
  selectedRows <- eventReactive(input$displayProductInfo_rows_selected, {
    # get index (or indices)
    currentRowIndex <- sort(input$displayProductInfo_rows_selected)
    
    # get selected data
    selectedRows <- dplyr::slice(
      isolate(reactiveData$productInfo), 
      sort(input$displayProductInfo_rows_selected)
    )
    
    return(list(
      index = currentRowIndex,
      table = selectedRows
    ))
  })
  
  # no we wanna create the UIoutput to edit product information
  output$editProductSummary <- renderUI({
    # only render when a row is selected (dont render when currentData changes)
    if (!is.null(input$displayProductInfo_rows_selected)) {
      prodInfo <- isolate(reactiveData$productInfo)
      selectizeInput(
        "productSummary", "Produkte Zusammenfassung",
        choices = unique(prodInfo$Produkte_Zusammenfassung),
        selected = selectedRows()$table$Produkte_Zusammenfassung[1],
        options = list(create = TRUE)
      )
    } # end of if condition
  }) # end of renderUI of editProductInf
  
  chosenProductSummary <- eventReactive(input$productSummary, {
    prodInfo <- isolate(reactiveData$productInfo)
    if (input$productSummary %in% unique(prodInfo$Produkte_Zusammenfassung)) {
      # take current row except it's full of empty strings
      # take current row for autofill
      currentRow <- dplyr::select(
        selectedRows()$table, Lieferant:Verpackungseinheit
      )
      if ( isTRUE(unique(nzchar(currentRow[1,]))) ) {
        # filter prodInfo by selected product summary and return other columns
        possibilites <- dplyr::filter(
          prodInfo, Produkte_Zusammenfassung == input$productSummary
        )
        
        ls <- list(
          deliverer1 = dplyr::pull(possibilites, Lieferant)[1],
          deliverer2 = dplyr::pull(possibilites, Lieferant2)[1],
          productGroup = dplyr::pull(possibilites, Produktgruppe)[1],
          bulksize = dplyr::pull(possibilites, Verpackungseinheit)[1]
        )
      } else {
        ls <- list(
          deliverer1 = dplyr::pull(currentRow, Lieferant)[1],
          deliverer2 = dplyr::pull(currentRow, Lieferant2)[1],
          productGroup = dplyr::pull(currentRow, Produktgruppe)[1],
          bulksize = dplyr::pull(currentRow, Verpackungseinheit)[1]
        )
      }
      
      
    } else {
      ls <- list(
        deliverer1 = "",
        deliverer2 = "",
        productGroup = "",
        bulksize = ""
      )
    }
    return(ls)
  })
  
  output$editAnyOtherInputColumns <- renderUI({
    prodInfo <- isolate(reactiveData$productInfo)
    inputColumns <- chosenProductSummary()
    
    if (!is.null(input$displayProductInfo_rows_selected)) {
      tagList(
        fluidRow(
          column(
            2,
            selectizeInput(
              "deliverer1", "Lieferant Nr.1",
              choices = unique(prodInfo$Lieferant),
              selected = inputColumns$deliverer1,
              options = list(create = TRUE)
            )
          ),
          column(
            2,
            selectizeInput(
              "deliverer2", "Lieferant Nr.2",
              choices = unique(prodInfo$Lieferant2),
              selected = inputColumns$deliverer2,
              options = list(create = TRUE)
            )
          ),
          column(
            3,
            selectizeInput(
              "productGroup", "Produktgruppe",
              choices = unique(prodInfo$Produktgruppe),
              selected = inputColumns$productGroup,
              options = list(create = TRUE)
            )
          ),
          column(
            2,
            selectizeInput(
              "bulksize", "VPE",
              choices = unique(prodInfo$Verpackungseinheit),
              selected = inputColumns$bulksize,
              options = list(create = TRUE)
            )
          )
        ), # end of first fluidRow
        # create action button
        fluidRow(
          column(
            2, offset = 2,
            actionButton(
              "saveButton", "Änderungen speichern", icon = icon("save")
            )
          )
        )
      ) # end of taglist
    }
  })
  
  # observe save button and if it's pressed, data will saved into database and 
  # reactiveData will be updated
  observeEvent(input$saveButton, {
    # everytime save button is pressed, update current row
    # get new values from the inputs
    updatedRow <- data.frame(
      Produkte_Zusammenfassung = input$productSummary,
      Lieferant = input$deliverer1,
      Lieferant2 = input$deliverer2,
      Produktgruppe = input$productGroup,
      Verpackungseinheit = input$bulksize,
      stringsAsFactors = FALSE
    )
    
    # update current row of productInfo except product's name in the app 
    # (= "Produkte_App")
    currentRowIndex <- input$displayProductInfo_rows_selected
    reactiveData$productInfo[currentRowIndex, -1] <- updatedRow[1, ]
  })
}) # end of server part

###################### execute shiny app ######################################
shinyApp(ui, server, options = list(port = 1234))