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
      
      # dataTable = mainPanel
      fluidRow(
        DT::dataTableOutput("table1")
      )
    ) # end of tabPanel == 1
  ) # end of navbarPage
) # end of shinyUI

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
  currentData <- eventReactive(input$tabs, {
    print(input$tabs)
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
  
  
  output$table1 <- DT::renderDataTable({
    if (is.data.frame(currentData())) {
    # show a datatable including product infos
      return(currentData())
    }
    if (is.list(currentData())) {
      # show a datatable with the current food storage without product infos
      data <- currentData()$originalData
      return(data)
    }
  })
  
  observeEvent(currentData(), {
    if (is.list(currentData())) {
      rV$productInfo <- currentData()$productInfo
      rV$addProducts <- currentData()$addProducts
    }
  })
  
}) # end of server part

###################### execute shiny app ######################################
shinyApp(ui, server, options = list(port = 1234))