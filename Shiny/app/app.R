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

add_label = function(df){
    df$label = paste0(
        '<b>', df$Country, '</b><br>
        <table style="width:120px;">
        <tr><td>Confirmed:</td><td align="right">', df$Confirmed, '</td></tr>
        <tr><td>Deceased:</td><td align="right">', df$Deaths, '</td></tr>
        <tr><td>Concluded:</td><td align="right">', df$Deaths+df$Recovered, '</td></tr>
        <tr><td>Active:</td><td align="right">', df$Confirmed-(df$Deaths+df$Recovered), '</td></tr>
        </table>'
    )
    
    df$label = lapply(df$label, HTML)
    
    return(df)
}

pal_confirmed = colorBin(
    palette = "viridis",
    domain = dat$Confirmed,
    bins = c(0,5000,10000,50000,100000,1000000,Inf),
    reverse = T
)

pal_recovered = colorNumeric(
    palette = "Greens",
    domain = dat$Recovered
)

pal_deaths = colorNumeric(
    palette = "Reds",
    domain = dat$Deaths
)

pal_cases_per_mil = colorNumeric(
    palette = "Browns",
    domain = dat$cases_per_mil
)


# Define UI for application that draws a histogram
ui = dashboardPage(
    dashboardHeader(
        title = "Assignment 4"
    ),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Map - Fill",
                     tabName = "side_by_side",
                     icon = icon("globe-africa")),
            menuItem("Data Info",
                     tabName = "data_info",
                     icon = icon("question-circle"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "side_by_side",
                    fluidRow(
                        column(
                            box(title = "",
                                withSpinner(leafletOutput("points"), type = 1),
                                width = 12
                            ),
                            width = 12
                        )#,
                        # column(
                        #     box(title = "",
                        #         withSpinner(leafletOutput("fill"), type = 1),
                        #         width = 12
                        #     ),
                        #     width = 6
                        # )
                    ),
                    fluidRow(
                        box(title = "",
                            sliderInput(
                                inputId = "date_slide",
                                label = "Select date",
                                min = min(dat$Date),
                                max = max(dat$Date),
                                value = max(dat$Date),
                                animate = animationOptions(loop = T, 
                                                           interval = 200),
                                width = "100%"
                            ), 
                            width = 12
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
            filter(Date == input$date_slide)%>%
            filter(Confirmed > 0) %>%
            add_label()
    })

    output$info = renderUI({
        tags$p("Download the latest dataset ", 
               tags$a(href = "https://raw.githubusercontent.com/datasets/covid-19/master/data/countries-aggregated.csv",
                      "here."), 
               br(), 
               br(), 
               "This dataset contains data up to ", max(dat$Date), ".")
    })
    
    # output$fill = renderLeaflet({
    #     p = leaflet() %>%
    #         setMaxBounds(-180, -90, 180, 90) %>%
    #         setView(lng = 0,
    #                 lat = 20,
    #                 zoom = 2) %>%
    #         addTiles(options = tileOptions(
    #             minZoom = 1
    #         )) %>%
    #         addLayersControl(
    #             baseGroups = c("Confirmed Cases",
    #                            "Deceased",
    #                            "Recovered",
    #                            "Cases per million")
    #         ) %>%
    #         hideGroup("Deceased") %>%
    #         hideGroup("Recovered") %>%
    #         hideGroup("Cases per million")
    # })
    
    output$points = renderLeaflet({
        p = leaflet() %>%
            setMaxBounds(-180, -90, 180, 90) %>%
            setView(lng = 0,
                    lat = 20, 
                    zoom = 2) %>%
            addTiles(options = tileOptions(
                minZoom = 1
            )) %>%
            addLayersControl(
                baseGroups = c("Confirmed Cases",
                               "Deceased",
                               "Recovered",
                               "Cases per million")
            ) %>%
            hideGroup("Deceased") %>%
            hideGroup("Recovered") %>%
            hideGroup("Cases per million")
    })
    
    observe({
        # req(input$animation_zoom)
        zoom_points = input$points_zoom
        
        # leafletProxy("fill", data = data_range()) %>%
        #     addPolygons(data = data_range()$geometry,
        #                 weight = 1,
        #                 fillOpacity = 0.2,
        #                 fillColor = ~colorQuantile("YlOrRd", domain = data_range()$Confirmed),
        #                 group = "Confirmed Cases")
        
        leafletProxy("points", data = data_range()) %>%
            clearMarkers() %>%
            addCircleMarkers(
                lng = ~X,
                lat = ~Y,
                radius = ~log2(Confirmed^(zoom_points/2)),
                stroke = F,
                fillOpacity = 0.6,
                label = ~label,
                group = "Confirmed Cases",
                color = ~pal_confirmed(Confirmed)
            ) %>%
            addCircleMarkers(
                lng = ~X,
                lat = ~Y,
                radius = ~log2(Deaths^(zoom_points/2)),
                stroke = F,
                fillOpacity = 0.6,
                label = ~label,
                group = "Deceased",
                color = ~pal_deaths(Deaths)
            ) %>%
            addCircleMarkers(
                lng = ~X,
                lat = ~Y,
                radius = ~log2(Recovered^(zoom_points/2)),
                stroke = F,
                fillOpacity = 0.6,
                label = ~label,
                group = "Recovered",
                color = ~pal_recovered(Recovered)
            ) %>%
            addCircleMarkers(
                lng = ~X,
                lat = ~Y,
                radius = ~log2(cases_per_mil^(zoom_points/2)),
                stroke = F,
                fillOpacity = 0.6,
                label = ~label,
                group = "Cases per million",
                color = ~pal_cases_per_mil(cases_per_mil)
            ) %>%
            addLegend(
                position = "bottomright",
                pal = pal_confirmed,
                values = ~Confirmed,
                group = "Deaths"
            )
            
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


# rsconnect::deployApp(getwd(), appName = "Assignment-4")
# rsconnect::deployApp("Shiny/app", appName = "Assignment-4")

