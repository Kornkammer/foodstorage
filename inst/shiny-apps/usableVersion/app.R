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
        selectInput(
          "filter",
          "Welche Produkte sollen angezeigt werden?",
          choices = filter
        )
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
    productInfo = datatable(matrix(c(1:10), nrow = 2)), # example data
    addProducts = c("example")
    # print(head(get("productInfo")))
  )
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
      
      return(list(
        editData = editData,
        productInfo = datatable(productInfo)
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
      
      newProducts <- bind_rows(productInfo, difDF)
      return(list(
        originalData = originalData,
        productInfo = datatable(productInfo),
        addProducts = dif,
        newProducts = newProducts
      ))
    }
  })
  # update reactive values
  observeEvent(currentData(), {
    if (is.list(currentData())) {
      rV$productInfo <- currentData()$productInfo
      rV$addProducts <- currentData()$addProducts
    }
  })
  
  
  #############################################################################
  ############# reactive part specially for foodstorage panel #################
  output$storage <- DT::renderDataTable({
    if (length(currentData()) == 2) {
      # show a datatable including product infos
      return(prepareDatatable(
        currentData()$editData,
        filter = input$filter
      ))
    }
    if (length(currentData()) == 4) {
      # show a datatable with the current food storage without product infos
      data <- currentData()$originalData
      return(data)
    }
  })
  
  #############################################################################
  ################## reactive part specially for product information ##########
  output$productInfo <- DT::renderDataTable({
    if (length(currentData()) == 2) {
      data <- currentData()$productInfo
    }
    if (length(currentData()) == 4) {
      data <- currentData()$newProducts
    }
    return(data)
  })
  
}) # end of server part

###################### execute shiny app ######################################
shinyApp(ui, server, options = list(port = 1234))