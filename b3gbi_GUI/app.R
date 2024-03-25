#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(b3gbi)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("B-Cubed: Biodiversity Indicators"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          fileInput('cube_file', 'Select the Data Cube File',
                    accept=c('text/csv','text/comma-separated-values,text/plain','.csv')),
          fileInput('species_info', 'Select the Species Info File',
                    accept=c('text/csv','text/comma-separated-values,text/plain','.csv')),
        ),

        # Show some metadata of the processed cube
        mainPanel(
          tabsetPanel(
            tabPanel("Summary",
                     br(),
                     br(),
                     textOutput("metadata"),
            ),
            tabPanel("Species",
                     br(),
                     br(),
                     textOutput("species_list"),
                     ),
            tabPanel("Occurrences",
                     br(),
                     br(),
                     DT::dataTableOutput("table"),

            )
          )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  options(shiny.maxRequestSize=30*1024^2)

 # cube_object = process_cube(input$cube_file$datapath, input$species_info$datapath)

  input_file <- reactive({
    if (is.null(input$cube_file)) {
      return("")
    }

    if (is.null(input$species_info)) {
      return("")
    }

    # actually read the file
    process_cube(input$cube_file$datapath, input$species_info$datapath)

  })

  output$metadata <- renderText({
    req(input_file())

    data <- input_file()

    paste("Number of species:", data$num_species)
  })

  output$species_list <- renderText({
    req(input_file())

    data <- input_file()
    species <- unique(data$data$scientificName)
    species
  })

  output$table <- DT::renderDataTable({

    # render only if there is data available
    req(input_file())

    data <- input_file()

    data$data
  })
}

# Run the application
shinyApp(ui = ui, server = server)
