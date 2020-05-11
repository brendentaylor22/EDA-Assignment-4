library(shiny)
library(ggplot2)
library(plotly)
library(shinydashboard)
library(lubridate)
library(dplyr)
library(sf)
library(shinycssloaders)
library(leaflet)
library(DT)
options(scipen=999)

#Read in data
#App loads much faster compared to updating data every time app is run
dat = readRDS("ncov-dat.rds")

#Remove geometry data to speed up processing
dat = dat %>%
    select(c("Date", "Country", "Confirmed", "Recovered", "Deaths", "cases_per_mil",
             "X", "Y", "deaths_per_closed", "pop"))

ncov_newest = readRDS("ncov-newest.rds")
ncov_newest = ncov_newest %>%
    select(c("Date", "Country", "Confirmed", "Recovered", "Deaths", "cases_per_mil",
             "X", "Y", "deaths_per_closed"))

#Find 5 countries with highest number of confirmed cases
dat_temp = dat %>%
    group_by(Country) %>%
    summarise(Confirmed = max(Confirmed)) %>%
    top_n(n = 5, wt = Confirmed)

temp_names = as.character(dat_temp$Country)

dat_5 = dat %>%
    filter(Country %in% temp_names) %>%
    group_by(Country, Date)

#Function to create label when country is hovered over in Leaflet map
#Code based on code by Dr Edward Parker & Quentin Leclerc (https://vac-lshtm.shinyapps.io/ncov_tracker/)
#https://github.com/eparker12/nCoV_tracker
add_label = function(df){
    df$label = paste0(
        '<b>', df$Country, '</b><br>
        <table style="width:120px;">
        <tr><td>Confirmed:</td><td align="right">', df$Confirmed, '</td></tr>
        <tr><td>Deceased:</td><td align="right">', df$Deaths, '</td></tr>
        <tr><td>Concluded:</td><td align="right">', df$Deaths+df$Recovered, '</td></tr>
        <tr><td>Active:</td><td align="right">', df$Confirmed-(df$Deaths+df$Recovered), '</td></tr>
        <tr><td>Population:</td><td align = "right">', df$pop, '</td></tr>
        </table>'
    )
    
    df$label = lapply(df$label, HTML)
    
    return(df)
}

#Functions used to define palettes for basegroups for Leaflet map
pal_confirmed = colorBin(
    palette = "viridis",
    domain = dat$Confirmed,
    bins = c(0,500,1000,5000,10000,50000,100000,1000000,Inf),
    reverse = T
)

pal_recovered = colorBin(
    palette = "viridis",
    domain = dat$Recovered,
    bins = c(0, 500, 1000, 10000, 100000, 150000, Inf),
    reverse = T
)

pal_deaths = colorBin(
    palette = "viridis",
    domain = dat$Deaths,
    bins = c(0, 50, 500, 1000, 10000, 50000, 100000, Inf),
    reverse = T
)

pal_cases_per_mil = colorBin(
    palette = "viridis",
    domain = dat$cases_per_mil,
    bins = c(0, 2, 5, 10, 50, 100, 1000, 5000, Inf),
    reverse = T
)

pal_deaths_per_inactive = colorBin(
    palette = "viridis",
    domain = dat$deaths_per_closed,
    bins = c(0, 2, 5, 10, 20, 50, 70, 100),
    reverse = T
)

# Define UI for application that draws a histogram
ui = dashboardPage( #Use shinydashboard package instead of standard layout
    dashboardHeader(
        title = "Assignment 4"
    ),
    dashboardSidebar(
        sidebarMenu( 
            #Add menu items
            menuItem("Map",
                     tabName = "map",
                     icon = icon("globe-africa")),
            menuItem("Graphs",
                     tabName = "graphs",
                     icon = icon("chart-bar")),
            menuItem("Data Table",
                     tabName = "data_table",
                     icon = icon("table")),
            menuItem("Data Info",
                     tabName = "data_info",
                     icon = icon("question-circle"))
        )
    ),
    dashboardBody(
        tabItems(
            #Map page
            tabItem(tabName = "map",
                    #First row has map
                    fluidRow(
                        column(
                            box(title = "",
                                #Include loading animation
                                withSpinner(leafletOutput("points"), type = 1),
                                width = 12
                            ),
                            width = 12
                        )
                    ),
                    #Second row has date slide
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
            #Data table page
            tabItem(tabName = "data_table",
                    h2("Data Table"),
                    fluidRow(
                        #First column is for filters (date range and country selection)
                        column(
                            box(title = "Filter",
                                dateRangeInput(inputId = "table_range",
                                               label = "Date Range",
                                               min = min(unique(dat$Date)),
                                               max = max(unique(dat$Date)),
                                               start = min(unique(dat$Date)),
                                               end = max(unique(dat$Date))
                                    
                                ),
                                selectInput(inputId = "table_country",
                                            label = "Country",
                                            choices = unique(dat$Country),
                                            multiple = T,
                                            ),
                                width = 12
                                
                            ),
                            width = 4
                        ),
                        #Second column is for data table
                        column(
                            DT::dataTableOutput("table"),
                            width = 8
                        )
                    )
                    
                    ),
            #Graphs page
            tabItem(tabName = "graphs",
                #First row is for country selection
                fluidRow(
                    column(
                        box(title = "Country",
                            selectInput(inputId = "plot_country",
                                        label = "",
                                        choices = unique(dat$Country),
                                        multiple = T,
                                        selected = dat_5$Country
                                        ),
                            width = 12,
                            ),
                        width = 12
                    )
                ),
                fluidRow(
                    column(
                        box(title = "Confirmed Cases",
                            withSpinner(plotlyOutput("cum_cases"), type = 1),
                            width = 12
                        ),
                        width = 6
                    ),
                    column(
                        box(title = "Deaths",
                            withSpinner(plotlyOutput("deaths"), type = 1),
                            width = 12
                        ),
                        width = 6
                    )
                ),
                fluidRow(
                    column(
                        box(title = "Recovered",
                            withSpinner(plotlyOutput("recovered"), type = 1),
                            width = 12
                        ),
                        width = 6
                    ),
                    column(
                        box(title = "Deaths Per Closed Cases",
                            withSpinner(plotlyOutput("deaths_per_closed"), type = 1),
                            width = 12
                        ),
                        width = 6
                    )
                )
            ),
            #Page to show when data was last updated
            tabItem(tabName = "data_info",
                    h2("Data Info"),
                    withSpinner(uiOutput("info"), type = 1)
                    )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #Reactive dataframe based on the date slider on the map page
    data_range = reactive({
        dat %>%
            filter(Date == input$date_slide)%>%
            filter(Confirmed > 0) %>%
            add_label()
    })
    
    #Reactive dataframe used to display data in the table
    #Based on the date range and the countries selected
    data_table = reactive({
        dat %>%
            select(c("Date", "Country", "Confirmed", "Recovered", "Deaths", "cases_per_mil")) %>%
            mutate(cases_per_mil = round(cases_per_mil, digits = 2)) %>%
            filter(between(Date, input$table_range[1], input$table_range[2])) %>%
            filter(if(is.null(input$table_country)) TRUE else Country %in% input$table_country)
    })
    
    #Reactive used to display graphs of countries selected
    data_countries = reactive({
        dat %>%
            filter(Country %in% input$plot_country)
    })
    
    #Data table output
    output$table = DT::renderDataTable(
        data_table(),
        options = list(
            pageLength = 20,
            dom = '<t>p' #Hides the search bar and includes page chooser at the bottom
        ),
        rownames = F, #Hide row names
        colnames = c("Date", "Country", "Confirmed", "Recovered", "Deaths", "Cases per million"),
        
    )

    #Basic text output to display dataset location and date data was updated
    output$info = renderUI({
        tags$p("Download the latest dataset ", 
               tags$a(href = "https://raw.githubusercontent.com/datasets/covid-19/master/data/countries-aggregated.csv",
                      "here."), 
               br(), 
               br(), 
               "This dataset contains data up to ", max(dat$Date), ".")
    })
    
    #Next few chunks are used to render the graphs on the Graphs page
    #They all use ggplotly and the data_countries() reactive dataframe
    output$cum_cases = renderPlotly({
        p = ggplot(data = data_countries()) +
                geom_line(aes(x = Date,
                             y = Confirmed,
                             color = Country)) +
            theme_minimal() +
            labs(y = "Confirmed Cases")
        
        ggplotly(p)
    })
    
    output$deaths = renderPlotly({
        p = ggplot(data = data_countries()) +
            geom_line(aes(x = Date,
                          y = Deaths,
                          color = Country)) +
            theme_minimal() +
            labs(y = "Deaths")
        
        ggplotly(p)
    })
    
    output$recovered = renderPlotly({
        p = ggplot(data = data_countries()) +
            geom_line(aes(x = Date,
                          y = Recovered,
                          color = Country)) +
            theme_minimal() +
            labs(y = "Recovered")
        
        ggplotly(p)
    })
    
    output$deaths_per_closed = renderPlotly({
        p = ggplot(data = data_countries()) +
            geom_line(aes(x = Date,
                          y = deaths_per_closed,
                          color = Country)) +
            theme_minimal() +
            labs(y = "Deaths Per Closed Case")
        
        ggplotly(p)
    })
    
    #Code to render leaflet map
    output$points = renderLeaflet({
        p = leaflet() %>%
            setMaxBounds(-180, -90, 180, 90) %>% 
            setView(lng = 0,
                    lat = 20, 
                    zoom = 2) %>% #Set default view and zoom
            addTiles(options = tileOptions(
                minZoom = 1 #Prevent user from scrolling too far out
            )) %>%
            #Add options to change metric that's displayed on the map
            addLayersControl(
                baseGroups = c("Confirmed cases",
                               "Deceased",
                               "Recovered",
                               "Cases per million",
                               "Deaths per inactive case")
            )
    })
    
    #Things inside this observe chunk are updated whenever the leaflet map is interacted with
    observe({
        #Get the current zoom level of the map
        #Used to adjust size of the circles as the user zooms in/out
        #Tried using addCircles() but it didn't look nearly as nice
        zoom_points = input$points_zoom
        
        leafletProxy("points", data = data_range()) %>%
            clearMarkers() %>%
            addCircleMarkers(
                lng = ~X,
                lat = ~Y,
                #Radius scale based on code by Christoph Schoenenberger (https://chschoenenberger.shinyapps.io/covid19_dashboard/)
                #https://github.com/chschoenenberger/covid19_dashboard/blob/master/sections/content_overview/map.R
                radius = ~log2(Confirmed^(zoom_points/2)), 
                stroke = F,
                fillOpacity = 0.6,
                label = ~label,
                group = "Confirmed cases",
                color = ~pal_confirmed(Confirmed)
            ) %>%
            addCircleMarkers(
                lng = ~X,
                lat = ~Y,
                #See reference above
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
                #See reference above
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
                #See reference above
                radius = ~log2(cases_per_mil^(zoom_points/2)),
                stroke = F,
                fillOpacity = 0.6,
                label = ~label,
                group = "Cases per million",
                color = ~pal_cases_per_mil(cases_per_mil)
            ) %>%
            addCircleMarkers(
                lng = ~X,
                lat = ~Y,
                #See reference above
                radius = ~log2(deaths_per_closed^(zoom_points)),
                stroke = F,
                fillOpacity = 0.6,
                label = ~label,
                group = "Deaths per inactive case",
                color = ~pal_deaths_per_inactive(deaths_per_closed)
            )
        
            
    })
    
    
    
    
    #########Leaflet Issue #477
    #Code is used to change the legend that is displayed on the map when a different basegroup is selected
    #Based on code by Emil Mahler Larsen in Leaflet Issue #477
    #https://github.com/rstudio/leaflet/issues/477#issuecomment-518984028
    observeEvent(input$points_groups,{
        leafletProxy("points") %>% 
            #Remove legends already on map
            removeControl(layerId = "Confirmed cases") %>% 
            removeControl(layerId = "Deceased") %>%
            removeControl(layerId = "Recovered") %>%
            removeControl(layerId = "Cases per million")
        
        #If 'Confirmed cases' is selected:
        #clear all legends and add new legend
        if ('Confirmed cases' %in% isolate(input$points_groups)){
            leafletProxy('points', data = data_range()) %>% 
                clearControls() %>%
                addLegend(position = "bottomright",
                          pal = pal_confirmed,
                          values = ~Confirmed)
        }
        else if ('Deceased' %in% isolate(input$points_groups)){
            leafletProxy('points', data = data_range()) %>% 
                clearControls() %>%
                addLegend(position = "bottomright",
                          pal = pal_deaths,
                          values = ~Deaths)
        }
        else if ('Recovered' %in% isolate(input$points_groups)){
            leafletProxy('points', data = data_range()) %>% 
                clearControls() %>%
                addLegend(position = "bottomright",
                          pal = pal_recovered,
                          values = ~Recovered)
        }
        else if ('Cases per million' %in% isolate(input$points_groups)){
            leafletProxy('points', data = data_range()) %>% 
                clearControls() %>%
                addLegend(position = "bottomright",
                          pal = pal_cases_per_mil,
                          values = ~cases_per_mil,
                          title = "Cases per million")
        }
        else if ('Deaths per inactive case' %in% isolate(input$points_groups)){
            leafletProxy('points', data = data_range()) %>% 
                clearControls() %>%
                addLegend(position = "bottomright",
                          pal = pal_deaths_per_inactive,
                          values = ~deaths_per_closed,
                          title = "Deaths per inactive case")
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


#Used to deploy to shinyapps.io
# rsconnect::deployApp("Shiny/app", appName = "Assignment-4")

