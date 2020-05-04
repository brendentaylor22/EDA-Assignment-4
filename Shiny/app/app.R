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
#library(plotly)
dat = readRDS("ncov-dat.rds")
ncov_newest = readRDS("ncov-newest.rds")

#dat = readRDS("Shiny/app/ncov-dat.rds")
#ncov_newest = readRDS("Shiny/app/ncov-newest.rds")

# Define UI for application that draws a histogram
ui = navbarPage("Main Title",
    tabPanel("Static Plot",
        fluidPage(
            titlePanel("Test"),
            sidebarLayout(sidebarPanel(
                # selectInput(
                #     inputId = "country",
                #     label = "Country",
                #     choices = dat$Country,
                #     selected = "South Africa",
                #     multiple = T),
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
                                     "points"))
            ),
            mainPanel(plotlyOutput("casesmap"))
            )
        )
    ),
    tabPanel("Test")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$casesmap = renderPlotly({
        if (input$display_options == "confirmed") {
            if(input$fill_options == "fill"){
                plot_cases_map = ggplot(data = ncov_newest) +
                    geom_sf(aes(geometry = geometry,
                            fill = Confirmed))
            }else{
                plot_cases_map = ggplot(data = ncov_newest) +
                    geom_sf(aes(geometry = geometry)) +
                    geom_point(aes(x = X, y = Y,
                        size = ifelse(Confirmed==0, NA, Confirmed)),
                        shape = 21,
                        colour = "turquoise",
                        alpha = 0.5,
                        fill = "blue")
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
                        fill = "blue")
            }else{
                plot_cases_map = ggplot(data = ncov_newest) +
                    geom_sf(aes(geometry = geometry,
                        fill = cases_per_mil))
            }
        }
        plot_cases_map
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
