#install.packages("shiny")
#install.packages("shinyWidgets")
#install.packages("devtools")
#devtools::install_github("shawndove/b3gbi")

#library(shiny)
#library(shinyWidgets)
#library(b3gbi)
#library(DT)
# Hello, can you see this?

ui <- fluidPage(
  # input = text fields, action buttons
  
  sidebarLayout(
    sidebarPanel(
      # input$dataCube
      fileInput(inputId = "dataCube",
                label = "Upload the data cube",
                ),
      # input$taxaFile
      fileInput(inputId = "taxaFile", label = "Upload the taxa information"),
      # shinyWidgetsGallery()
      
    ),
    # output = tables, plots, texts  
    mainPanel(
      tabsetPanel(
        tabPanel(title = "Metadata",
                 ## output$metadata
                 textOutput("metadata")
        ),
        
        tabPanel(title = "Plot",
                 ## output$plot
                 plotOutput("plot"),
                 textOutput("dataCube_path"),
                 textOutput("taxaFile_path")
        ),
        tabPanel(title = "Table",
                 DTOutput("table")
        ),
        tabPanel(title = "Report",
                 textOutput("name")
        )
      ),
    )
  )  
  
  # shinyWidgetsGallery()
  
  
  
)

server <-function(input, output){
  
  dataCube <- reactive({
    # Load GBIF data cube
    # cube_name <- "data/europe_species_cube.csv"
    cube_name <- input$dataCube$datapath
    
    # Load taxonomic info for cube
    # tax_info <- "data/europe_species_info.csv"
    tax_info <- input$taxaFile$datapath
    
    # Prepare cube
    insect_data <- process_cube(cube_name, tax_info)
    
    insect_data
  })
  
  output$table <- renderDT({
    dataCube()$data
  })
  
  
  output$metadata <- renderText(
    paste("Hello,", input$metadata)
  )
  
  output$plot <- renderPlot({
    # Calculate diversity metric
    map_obs_rich_insects <- obs_richness_map(dataCube())
    
    # Plot diversity metric
    plot(map_obs_rich_insects, title = "Observed Species Richness: Insects in Europe") 
  })
  
}

shinyApp(ui = ui, server = server)
