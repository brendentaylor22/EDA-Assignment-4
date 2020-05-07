library(shiny)
library(ggplot2)
library(plotly)
library(shinydashboard)
library(lubridate)
library(dplyr)
library(sf)
library(shinycssloaders)
options(scipen=999)

dat = readRDS("ncov-dat.rds")
ncov_newest = readRDS("ncov-newest.rds")

#dat = readRDS("Shiny/app/ncov-dat.rds")
#ncov_newest = readRDS("Shiny/app/ncov-newest.rds")

# Define UI for application that draws a histogram
ui = dashboardPage(
    dashboardHeader(
        title = "Assignment 4"
    ),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Static Plot",
                     tabName = "test",
                     icon = icon("globe-africa")),
            menuItem("Animation",
                     tabName = "animation",
                     icon = icon("play-circle")),
            menuItem("Data Info",
                     tabName = "data_info",
                     icon = icon("question-circle"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "test",
                fluidRow(
                    box(title = "Options",
                        dateInput(
                            inputId = "date",
                            label = "Date",
                            min = min(dat$Date),
                            max = max(dat$Date),
                            value = max(dat$Date)
                        ),
                        width = 4
                        ),
                    box(title = "",
                        withSpinner(plotlyOutput("casesmap"), type = 1),
                        width = 8
                    )
                ),
                fluidRow(
                    box(column(width = 6,
                        title = "Options",
                        radioButtons(
                            inputId = "metric_options",
                            label = "Metric",
                            choiceNames = c("Confirmed cases",
                                            "Cases per million citizens"),
                            choiceValues = c("confirmed",
                                             "cases_per_mil"))
                        ),
                        column(width = 6,
                        radioButtons(
                            inputId = "display_options",
                            label = "Display Options",
                            choiceNames = c("Fill",
                                            "Points"),
                            choiceValues = c("fill",
                                             "points"))),
                        width = 12
                    )
                )
            ),
            tabItem(tabName = "animation",
                    h2("Widgets tab content")),
            tabItem(tabName = "data_info",
                    h2("Data Info"),
                    withSpinner(uiOutput("info"), type = 1)
                    )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    data = reactive({
        dat %>%
            filter(Date == input$date)
    })
    
    output$info = renderUI({
        tags$p("Download the latest dataset ", 
               tags$a(href = "https://raw.githubusercontent.com/datasets/covid-19/master/data/countries-aggregated.csv",
                      "here."), 
               br(), 
               br(), 
               "This dataset contains data up to ", max(dat$Date), ".")
    })
    
    output$casesmap = renderPlotly({
        if (input$metric_options == "confirmed") {
            if(input$display_options == "fill"){
                plot_cases_map = ggplot(data = data()) +
                    geom_sf(aes(geometry = geometry,
                            fill = Confirmed)) +
                    theme_minimal()
            }else{
                plot_cases_map = ggplot(data = data()) +
                    geom_sf(aes(geometry = geometry)) +
                    geom_point(aes(x = X, y = Y,
                        size = ifelse(Confirmed==0, NA, Confirmed)),
                        shape = 21,
                        colour = "turquoise",
                        alpha = 0.5,
                        fill = "blue") +
                    theme_minimal()
            }
        }else{
            if(input$display_options == "points"){
                plot_cases_map = ggplot(data = data()) +
                    geom_sf(aes(geometry = geometry)) +
                    geom_point(aes(x = X, y = Y,
                        size = ifelse(cases_per_mil==0, NA, cases_per_mil)),
                        shape = 21,
                        colour = "turquoise",
                        alpha = 0.5,
                        fill = "blue") +
                    theme_minimal()
            }else{
                plot_cases_map = ggplot(data = data()) +
                    geom_sf(aes(geometry = geometry,
                        fill = cases_per_mil)) +
                    theme_minimal()
            }
        }
        plot_cases_map
        
        
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)


# rsconnect::deployApp(getwd(), appName = "Assignment-4")
