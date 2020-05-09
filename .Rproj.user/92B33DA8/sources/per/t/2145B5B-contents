library(shiny)
library(ggplot2)
library(plotly)
library(shinydashboard)
library(lubridate)
library(dplyr)
library(sf)
library(shinycssloaders)
#library(gganimate)
library(leaflet)
library(DT)
options(scipen=999)

dat = readRDS("ncov-dat.rds")
dat = dat %>%
    select(c("Date", "Country", "Confirmed", "Recovered", "Deaths", "cases_per_mil",
             "X", "Y"))

ncov_newest = readRDS("ncov-newest.rds")
ncov_newest = ncov_newest %>%
    select(c("Date", "Country", "Confirmed", "Recovered", "Deaths", "cases_per_mil",
             "X", "Y"))

dat_temp = dat %>%
    group_by(Country) %>%
    summarise(Confirmed = max(Confirmed)) %>%
    top_n(n = 5, wt = Confirmed)

temp_names = as.character(dat_temp$Country)

dat_5 = dat %>%
    filter(Country %in% temp_names) %>%
    group_by(Country, Date)

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

top_5 = ncov_newest %>%
    select(c("Date", "Country", "Confirmed", "Recovered", "Deaths", "cases_per_mil")) %>%
    top_n(n = 5, Confirmed) %>%
    select(c("Country"))




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
            menuItem("Data Table",
                     tabName = "data_table",
                     icon = icon("table")),
            menuItem("Graphs",
                     tabName = "graphs",
                     icon = icon("chart-bar")),
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
            tabItem(tabName = "data_table",
                    h2("Data Table"),
                    fluidRow(
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
                        column(
                            DT::dataTableOutput("table"),
                            width = 8
                        )
                    )
                    
                    ),
            tabItem(tabName = "graphs",
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
                        width = 6
                    )
                ),
                fluidRow(
                    column(
                        box(title = "Confirmed Cases",
                            withSpinner(plotlyOutput("cum_cases"), type = 1),
                            width = 12
                        ),
                        width = 6
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
    
    data_range = reactive({
        dat %>%
            filter(Date == input$date_slide)%>%
            filter(Confirmed > 0) %>%
            add_label()
    })
    
    data_table = reactive({
        dat %>%
            select(c("Date", "Country", "Confirmed", "Recovered", "Deaths", "cases_per_mil")) %>%
            mutate(cases_per_mil = round(cases_per_mil, digits = 2)) %>%
            filter(between(Date, input$table_range[1], input$table_range[2])) %>%
            filter(if(is.null(input$table_country)) TRUE else Country %in% input$table_country)
    })
    
    data_cases = reactive({
        dat %>%
            filter(Country %in% input$plot_country)
    })
    
    output$table = DT::renderDataTable(
        data_table(),
        options = list(
            pageLength = 20,
            dom = '<t>p'
        ),
        rownames = F,
        colnames = c("Date", "Country", "Confirmed", "Recovered", "Deaths", "Cases per million"),
        
    )

    output$info = renderUI({
        tags$p("Download the latest dataset ", 
               tags$a(href = "https://raw.githubusercontent.com/datasets/covid-19/master/data/countries-aggregated.csv",
                      "here."), 
               br(), 
               br(), 
               "This dataset contains data up to ", max(dat$Date), ".")
    })
    
    output$cum_cases = renderPlotly({
        p = ggplot(data = data_cases()) +
                geom_line(aes(x = Date,
                             y = Confirmed,
                             color = Country)) +
            theme_minimal() +
            labs(y = "Confirmed Cases")
        
        ggplotly(p)
        
       # plot_ly(data = data_cum_cases(),
       #         x = ~Date,
       #         y = ~Confirmed,
       #         type = "scatter",
       #         mode = "lines",
       #         color = ~as.factor(Country)
       #  )
    })
    
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
            ) #%>%
            # addLegend(
            #     position = "bottomright",
            #     pal = pal_confirmed,
            #     values = ~Confirmed
            # )
            
    })
    
    
    
    
    #########Leaflet Issue #477
    observeEvent(input$points_groups,{
        leafletProxy("points") %>% 
            removeControl(layerId = "Confirmed Cases") %>% 
            removeControl(layerId = "Deceased") %>%
            removeControl(layerId = "Recovered") %>%
            removeControl(layerId = "Cases per million")
        
        if ('Confirmed Cases' %in% isolate(input$points_groups)){
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
                          values = ~cases_per_mil)
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


# rsconnect::deployApp(getwd(), appName = "Assignment-4")
# rsconnect::deployApp("Shiny/app", appName = "Assignment-4")

