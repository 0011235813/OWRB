library(shiny)        #Provides the framework to build interactive web apps
library(shinyWidgets) #For enhanced UI widgets, like pickerInput with checkboxes
library(dplyr)        #For data manipulation using pipes (%>%) and verbs like filter, select
library(dbplyr)       #Allows dplyr syntax to query databases lazily (remote computation)
library(lubridate)    #Makes working with dates easier (e.g., Sys.Date() - years(1))
library(DT)           #Renders interactive tables with scrolling, sorting, and paging
library(leaflet)      #for map
library(shinyjs)      #for sidebar toggle / hide-show UI
library(readxl)       #for xlsx uploads
library(readr)        #for csv/tsv uploads

#-------------------------------
#SOURCE CONNECTION SCRIPT
#-------------------------------
#This script contains helper functions to connect to AWQMS, retrieve tables, and disconnect safely
#It may also include authentication logic, retries, or other connection-related helpers
#Example: awqms_get_con() establishes a live connection

#source("I:/WaterQuality/Groundwater/Data Management/Database Connections/AWQMS_LINK/Scripts/AWQMS_link_Source_Develope.R")
source("C:/Users/364483.AGENCY/OneDrive - State of Oklahoma/Documents/R code/AWQMS_Source_Code_02102026_v2.R")

#-------------------------------
#UI
#-------------------------------
ui <- fluidPage(
  useShinyjs(),

  #-------------------------------
  #Custom Banner
  #-------------------------------
  div(
    style = "background-color: #006400; padding: 10px; display: flex; align-items: center; justify-content: space-between;",

    #App title
    h2("NRSA Prototype QA App", style = "margin: 0; color: white;"),

    #OWRB Logo (replace src with the correct path or URL)
    img(src = "LakeViewBackground1023.jpg",
        height = "50px")
  ),

  br(),

  # Source-specific controls
  uiOutput("top_controls"),

  br(),

  #-------------------------------
  #Main Content Tabs (Full Width)
  #-------------------------------
  div(
    style = "width: 100vw; margin: 0; padding: 0;",
    tabsetPanel(
      id = "main_tabs",

      #-------------------------------
      #Summary Tab (Full Width)
      #-------------------------------
      tabPanel(
        "Summary",
        div(
          style = "width: 100vw; padding: 0; margin: 0; overflow: auto;",
          DTOutput("table", width = "100%")
        )
      ),

      #-------------------------------
      #Site Map Tab
      #-------------------------------
      tabPanel(
        "Site Map",
        div(
          style = "width: 100%; height: 100vh; padding: 0;",
          leafletOutput("site_map", width = "100%", height = "100%")
        )
      )
    )
  )
)

#-------------------------------
#SERVER
#-------------------------------
server <- function(input, output, session) {

  data_source <- reactiveVal(NULL) # "odbc" or "upload"
  con <- reactiveVal(NULL)

  showModal(
    modalDialog(
      title = "Choose Data Source",
      p("Would you like to proceed with a file upload or use the ODBC connection?"),
      footer = tagList(
        actionButton("choose_upload", "File Upload"),
        actionButton("choose_odbc", "ODBC Connection")
      ),
      easyClose = FALSE
    )
  )

  observeEvent(input$choose_upload, {
    data_source("upload")
    removeModal()
    updateTabsetPanel(session, inputId = "main_tabs", selected = "Summary")
  })

  observeEvent(input$choose_odbc, {
    data_source("odbc")
    removeModal()
    updateTabsetPanel(session, inputId = "main_tabs", selected = "Summary")
  })

  output$top_controls <- renderUI({
    req(data_source())

    if (identical(data_source(), "upload")) {
      fluidRow(
        column(
          8,
          fileInput(
            inputId = "upload_files",
            label = "Upload one or more data files (csv, tsv, xlsx)",
            multiple = TRUE,
            accept = c(".csv", ".tsv", ".txt", ".xlsx", ".xls")
          )
        ),
        column(
          2,
          br(),
          actionButton("apply_upload", "Load Uploaded Files", width = "100%")
        )
      )
    } else {
      fluidRow(
        column(2,
               dateInput("start_date", "Start Date", value = Sys.Date() - years(1))
        ),
        column(2,
               dateInput("end_date", "End Date", value = Sys.Date())
        ),
        column(3,
               selectInput(
                 inputId = "protocol",
                 label = "Protocol",
                 choices = c(
                   "NRSA Protocol",
                   "State Protocol"
                 ),
                 selected = "NRSA Protocol"
               )
        ),
        column(3,
               pickerInput(
                 inputId = "project",
                 label = "Project",
                 choices = NULL,
                 multiple = TRUE,
                 options = list(
                   `actions-box` = TRUE,
                   `live-search` = TRUE,
                   `selected-text-format` = "count > 3",
                   `tick-icon` = "glyphicon glyphicon-ok"
                 )
               )
        ),
        column(2,
               br(),
               actionButton("apply_filters", "Apply Filters", width = "100%")
        )
      )
    }
  })

  #---- Connect to AWQMS only if ODBC workflow is selected ----
  observeEvent(input$choose_odbc, {
    if (is.null(con())) {
      con(awqms_get_con())
    }
  }, once = TRUE)

  onStop(function() {
    existing_con <- isolate(con())
    if (!is.null(existing_con)) {
      awqms_disconnect()
    }
  })

  #---- Lazy table reference ----
  results_tbl <- reactive({
    req(data_source() == "odbc")
    req(!is.null(con()))

    tbl(con(), "results_standard_vw") %>%
      filter(
        activity_media == "Habitat",
        monitoring_location_type == "River/Stream"
      )
  })

  #---- Cache lookup tables ----
  lookup_cache <- reactiveValues()

  #---- Populate Project choices ----
  observe({
    req(data_source() == "odbc")

    projects_all <- results_tbl() %>%
      select(starts_with("project_id")) %>%
      distinct() %>%
      collect()

    project_vector <- unique(unlist(projects_all))
    project_vector <- project_vector[!is.na(project_vector)]

    updatePickerInput(session, "project",
                      choices = project_vector)
  })

  #-------------------------------------------
  #Shared summary builder
  #-------------------------------------------
  build_summary_table <- function(data) {
    #-------------------------------------------
    #Parent Activity ID logic
    #-------------------------------------------
    data <- data %>%
      mutate(parent_activity_id = sub(":.*$", "", activity_id))

    #-------------------------------------------
    #Transect summary (UNCHANGED)
    #-------------------------------------------
    transects <- data %>%
      filter(!is.na(sampling_component_name) & sampling_component_name != "") %>%
      group_by(parent_activity_id) %>%
      summarize(
        transect_summary = paste(sort(unique(sampling_component_name)), collapse = "-"),
        .groups = "drop"
      ) %>%
      mutate(
        transect_complete = ifelse(
          transect_summary == paste(LETTERS[1:11], collapse = "-"),
          "✔",
          "✖"
        )
      )

    #-------------------------------------------
    #FIX: TRUE one row per parent_activity_id
    #-------------------------------------------
    parent_meta <- data %>%
      group_by(parent_activity_id) %>%
      summarize(
        monitoring_location_id   = first(monitoring_location_id),
        monitoring_location_name = first(monitoring_location_name),
        activity_start_date      = as.Date(first(activity_start_date)),
        sample_collection_method_id   = first(sample_collection_method_id),
        sample_collection_method_name = first(sample_collection_method_name),
        activity_latitude  = first(activity_latitude),
        activity_longitude = first(activity_longitude),
        .groups = "drop"
      )

    #-------------------------------------------
    #Combine
    #-------------------------------------------
    parent_meta %>%
      left_join(transects, by = "parent_activity_id") %>%
      mutate(
        transect_summary = ifelse(is.na(transect_summary), "No Transects", transect_summary),
        transect_complete = ifelse(is.na(transect_complete), "", transect_complete)
      ) %>%
      select(
        parent_activity_id,
        transect_summary,
        transect_complete,
        monitoring_location_id,
        monitoring_location_name,
        activity_start_date,
        sample_collection_method_id,
        sample_collection_method_name,
        activity_latitude,
        activity_longitude
      )
  }

  #-------------------------------------------
  #Upload loader
  #-------------------------------------------
  uploaded_data <- eventReactive(input$apply_upload, {
    req(data_source() == "upload")
    req(input$upload_files)

    withProgress(message = "Loading uploaded files...", value = 0, {
      files <- input$upload_files
      n <- nrow(files)

      loaded <- lapply(seq_len(n), function(i) {
        f <- files$datapath[i]
        ext <- tolower(tools::file_ext(files$name[i]))

        incProgress(1 / max(n, 1), detail = paste("Reading", files$name[i]))

        if (ext %in% c("csv")) {
          readr::read_csv(f, show_col_types = FALSE)
        } else if (ext %in% c("tsv", "txt")) {
          readr::read_tsv(f, show_col_types = FALSE)
        } else if (ext %in% c("xlsx", "xls")) {
          readxl::read_excel(f)
        } else {
          stop(paste("Unsupported file type:", files$name[i]))
        }
      })

      bind_rows(loaded)
    })
  })

  #-------------------------------------------
  #Filtered Data (ODBC logic UNCHANGED)
  #-------------------------------------------
  filtered_odbc_data <- eventReactive(input$apply_filters, {
    req(data_source() == "odbc")

    withProgress(message = "Loading data, please wait...", value = 0, {

      q <- results_tbl() %>%
        filter(activity_media == "Habitat",
               monitoring_location_type == "River/Stream")

      incProgress(0.2)

      if (!is.null(input$start_date)) {
        q <- q %>% filter(activity_start_date >= as.Date(input$start_date))
      }
      if (!is.null(input$end_date)) {
        q <- q %>% filter(activity_start_date <= as.Date(input$end_date))
      }

      incProgress(0.2)

      #-------------------------------------------
      #Protocol Filtering
      #-------------------------------------------

      if (input$protocol == "NRSA Protocol") {

        #Currently ALL methods are NRSA
        #Later you can replace this with a specific vector

        q <- q %>%
          filter(sample_collection_method_id %in% c(
            "Wadeable",
            "Boatable",
            "DRYVISIT",
            "INTWADE",
            "PARBYBOAT",
            "PARBYWADE"
            #Add others here as needed
          ))

      }

      if (input$protocol == "State Protocol") {

        #Placeholder (currently none)
        q <- q %>%
          filter(sample_collection_method_id %in% c(
            "State_Method1",
            "State_Method2"
          ))

      }

      if (length(input$project)) {
        q <- q %>% filter(
          project_id1 %in% input$project |
            project_id2 %in% input$project |
            project_id3 %in% input$project |
            project_id4 %in% input$project |
            project_id5 %in% input$project |
            project_id6 %in% input$project
        )
      }

      incProgress(0.2)

      data <- q %>% collect()
      incProgress(0.4)

      build_summary_table(data)
    })
  })

  #-------------------------------------------
  #Final data switcher
  #-------------------------------------------
  filtered_data <- reactive({
    if (identical(data_source(), "upload")) {
      build_summary_table(uploaded_data())
    } else {
      filtered_odbc_data()
    }
  })

  #-------------------------------------------
  #Render Table (UNCHANGED)
  #-------------------------------------------
  output$table <- renderDT({
    validate(
      need(!is.null(data_source()), "Choose a data source to get started."),
      need(!(identical(data_source(), "upload") && is.null(input$upload_files)),
           "Choose File Upload, select one or more files, and click 'Load Uploaded Files'."),
      need(!(identical(data_source(), "odbc") && input$apply_filters < 1),
           "Choose ODBC Connection and click 'Apply Filters'.")
    )

    filtered_data()
  },
  rownames = FALSE,
  options = list(
    scrollX = TRUE,
    scrollY = NULL,   #let div handle height and scrolling
    fixedHeader = TRUE,
    autoWidth = TRUE,
    pageLength = 25,
    responsive = TRUE,
    columnDefs = list(
      list(visible = FALSE, targets = c(8, 9)) #hide lat/long
    )
  ))

  #-------------------------------------------
  #Selected Site Reactive (UNCHANGED)
  #-------------------------------------------
  selected_site <- reactive({
    req(input$table_rows_selected)
    filtered_data()[input$table_rows_selected, ]
  })

  observeEvent(input$table_rows_selected, {
    updateTabsetPanel(
      session,
      inputId = "main_tabs",
      selected = "Site Map"
    )
  })

  #-------------------------------------------
  #Map Render (UNCHANGED)
  #-------------------------------------------
  output$site_map <- renderLeaflet({
    req(selected_site())

    site <- selected_site()

    leaflet() %>%
      addProviderTiles("Esri.WorldTopoMap") %>%
      setView(
        lng = site$activity_longitude,
        lat = site$activity_latitude,
        zoom = 13
      ) %>%
      addMarkers(
        lng = site$activity_longitude,
        lat = site$activity_latitude,
        popup = paste0(
          "<b>Parent Activity ID:</b> ", site$parent_activity_id, "<br>",
          "<b>Location:</b> ", site$monitoring_location_name, "<br>",
          "<b>Transects:</b> ", site$transect_summary
        )
      )
  })
}

#-------------------------------
#RUN THE APP
#-------------------------------
shinyApp(ui, server)
