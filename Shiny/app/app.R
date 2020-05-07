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
library(shinydashboard)

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
                     icon = icon("dashboard")),
            menuItem("New Page",
                     tabName = "new_page",
                     icon = icon("th"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "test",
                fluidRow(
                    box(title = "Options",
                        radioButtons(
                            inputId = "display_options",
                            label = "Display Options",
                            choiceNames = c("Confirmed cases",
                                            "Cases per million citizens"),
                            choiceValues = c("confirmed",
                                             "cases_per_mil")),
                        radioButtons(
                            inputId = "fill_options",
                            label = "Fill Options",
                            choiceNames = c("Fill",
                                            "Points"),
                            choiceValues = c("fill",
                                             "points")),
                        width = 4
                        ),
                    box(title = "",
                        plotlyOutput("casesmap"),
                        width = 8
                    )
                )
            ),
            tabItem(tabName = "new_page",
                    h2("Widgets tab content"))
        )
    )
)





# ui = navbarPage("Main Title",
#     tabPanel("Static Plot",
#         fluidPage(
#             titlePanel("Test"),
#             sidebarLayout(sidebarPanel(
#                 # selectInput(
#                 #     inputId = "country",
#                 #     label = "Country",
#                 #     choices = dat$Country,
#                 #     selected = "South Africa",
#                 #     multiple = T),
#                 radioButtons(
#                     inputId = "display_options",
#                     label = "Display Options",
#                     choiceNames = c("Confirmed cases",
#                                     "Cases per million citizens"),
#                     choiceValues = c("confirmed", 
#                                      "cases_per_mil")),
#                 radioButtons(
#                     inputId = "fill_options",
#                     label = "Fill Options",
#                     choiceNames = c("Fill",
#                                     "Points"),
#                     choiceValues = c("fill", 
#                                      "points"))
#             ),
#             # sidebarPanel(
#             #     dateInput(
#             #         inputId = "date_select",
#             #         label = "Date",
#             #         value = max(ncov_newest$Date),
#             #         min = min(ncov_newest$Date),
#             #         max = max(ncov_newest$Date),
#             #         autoclose = T
#             #     )
#             # ),
#             
#             mainPanel(plotlyOutput("casesmap"))
#             ),
#         )
#     ),
#     tabPanel("Test")
# )

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$casesmap = renderPlotly({
        if (input$display_options == "confirmed") {
            if(input$fill_options == "fill"){
                plot_cases_map = ggplot(data = ncov_newest) +
                    geom_sf(aes(geometry = geometry,
                            fill = Confirmed)) +
                    theme_minimal()
            }else{
                plot_cases_map = ggplot(data = ncov_newest) +
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
            if(input$fill_options == "points"){
                plot_cases_map = ggplot(data = ncov_newest) +
                    geom_sf(aes(geometry = geometry)) +
                    geom_point(aes(x = X, y = Y,
                        size = ifelse(cases_per_mil==0, NA, cases_per_mil)),
                        shape = 21,
                        colour = "turquoise",
                        alpha = 0.5,
                        fill = "blue") +
                    theme_minimal()
            }else{
                plot_cases_map = ggplot(data = ncov_newest) +
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


# rsconnect::deployApp("Shiny/app", appName = "Assignment-4")
