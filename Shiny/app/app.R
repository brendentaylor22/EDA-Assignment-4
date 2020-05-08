library(shiny)
library(ggplot2)
library(plotly)
library(shinydashboard)
library(lubridate)
library(dplyr)
library(sf)
library(shinycssloaders)
library(gganimate)
library(leaflet)
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
                                            "Cases per million citizens",
                                            "Mortality Rate of Completed Cases"),
                            choiceValues = c("confirmed",
                                             "cases_per_mil",
                                             "deaths_per_completed"))
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
                fluidRow(
                    box(title = "Options",
                        dateRangeInput(
                            inputId = "date_range",
                            label = "Date Range",
                            start = min(dat$Date),
                            end = max(dat$Date),
                            min = min(dat$Date),
                            max = max(dat$Date),
                            autoclose = T
                        ),
                        width = 4
                    ),
                    box(title = "",
                        withSpinner(leafletOutput("animation"), type = 1),
                        width = 8
                    )
                )
            ),
            tabItem(tabName = "data_info",
                    h2("Data Info"),
                    withSpinner(uiOutput("info"), type = 1)
                    )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    geom_dat = dat$geometry
    
    data_date = reactive({
        dat %>%
            filter(Date == input$date) 
    })
    
    data_range = reactive({
        dat %>%
            # filter(between(Date, input$date_range[1], input$date_range[2]))
            filter(Date == input$date_range[1])%>%
            filter(Confirmed > 0)
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
        p = ggplot(data = data_date())
        if (input$metric_options == "confirmed") {
            if(input$display_options == "fill"){
                p = p +
                    geom_sf(aes(geometry = geometry,
                            fill = Confirmed)) +
                    theme_minimal()
            }else{
                p = p +
                    geom_sf(aes(geometry = geometry)) +
                    geom_point(aes(x = X, y = Y,
                        size = ifelse(Confirmed==0, NA, Confirmed)),
                        shape = 21,
                        colour = "turquoise",
                        alpha = 0.5,
                        fill = "blue") +
                    theme_minimal()
            }
        }else if(input$metric_options == "cases_per_mil"){
            if(input$display_options == "points"){
                p = p +
                    geom_sf(aes(geometry = geometry)) +
                    geom_point(aes(x = X, y = Y,
                        size = ifelse(cases_per_mil==0, NA, cases_per_mil)),
                        shape = 21,
                        colour = "turquoise",
                        alpha = 0.5,
                        fill = "blue") +
                    theme_minimal()
            }else{
                p = p +
                    geom_sf(aes(geometry = geometry,
                        fill = cases_per_mil)) +
                    theme_minimal()
            }
        } else{
            if(input$display_options == "points"){
                p = p +
                    geom_sf(aes(geometry = geometry)) +
                    geom_point(aes(x = X, y = Y,
                                   size = ifelse(deaths_per_closed==0, NA, deaths_per_closed)),
                               shape = 21,
                               colour = "turquoise",
                               alpha = 0.5,
                               fill = "blue") +
                    theme_minimal()
            }else{
                p = p +
                    geom_sf(aes(geometry = geometry,
                                fill = deaths_per_closed)) +
                    theme_minimal()
            }
        }
        p
        
        
    })
    
    # output$animation = renderPlotly({
    #     p = ggplot(data = data_range())
    #     
    #     p = p +
    #         geom_sf(aes(geometry = geometry)) +
    #         geom_point(aes(x = X, y = Y,
    #                        size = ifelse(Confirmed==0, NA, Confirmed),
    #                        frame = as.factor(Date)),
    #                    shape = 21,
    #                    alpha = 0.5) +
    #         theme_minimal()
    #     
    #     ggplotly(p)
    
    output$animation = renderLeaflet({
        p = leaflet() %>%
            setMaxBounds(-180, -90, 180, 90) %>%
            setView(lng = 0,
                    lat = 20, 
                    zoom = 2) %>%
            addTiles()
            # addCircles(lng = ~X,
            #            lat = ~Y,
            #            data = data_range(),
            #            stroke = F,
            #            fillOpacity = 0.5,
            #            radius = ~ifelse(Confirmed == 0, NA, Confirmed))
        
        # p
    })
    
    observe({
        # req(input$animation_zoom)
        zoom_level = input$animation_zoom
        
        leafletProxy("animation", data = data_range()) %>%
            addCircleMarkers(
                lng = ~X,
                lat = ~Y,
                # radius = ~ifelse(Confirmed == 0, 
                #                  NA, 
                #                  log(Confirmed^(zoom_level/2))),
                radius = ~log(Confirmed^(zoom_level/2)),
                stroke = F,
                fillOpacity = 0.6
            )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


# rsconnect::deployApp(getwd(), appName = "Assignment-4")
