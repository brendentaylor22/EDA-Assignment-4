#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(plotly)
dat = readRDS("ncov-dat.rds")
ncov_newest = readRDS("ncov-newest.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Test"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(sidebarPanel(
            selectInput(
            inputId = "country",
            label = "Country",
            choices = dat$Country,
            selected = "South Africa",
            multiple = T
        )
    ), 

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("casesmap")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$casesmap = renderPlotly({
        plot_cases_map = ggplot(data = ncov_newest) +
            geom_sf(aes(geometry = geometry),
                    fill = ifelse("Vatican" %in% input$country, "red", "blue"))
        plot_cases_map
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
