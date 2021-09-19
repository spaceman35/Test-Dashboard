# This R Shiny Dashboard is...
# Survey tool POC: Maj Sean Kelly, AFOTEC Det 5, sean.kelly.33@spaceforce.mil
# Database POC: Lt Tim Dawson, AFOTEC Det 5,

## ---------------------------------------------------------------------------------------- ##
# Before we get to the server.R and ui.R portions, these are things that could go in a global.R file, but to minimize the number of files, I included these sections here.

library(shiny)
library(shinydashboard)
# library(semantic.dashboard) #This might be useful if we want to make it look much prettier/customizable than just a skin color
# the themes are at https://semantic-ui-forest.com/themes
library(DT) #datatable package

## ---------------------------------------------------------------------------------------- ##
# This is the UI section of the app.R file. This tells the shiny server how to display the survey.
# For now, I have it displaying a very simple .html file, and the survey the user selects is the "survey" output
ui <- dashboardPage(skin = "black", #if using shinydashboard
# ui <- dashboardPage( theme = "slate", #if using semantic.dashboard
# Icons can be found here: https://fontawesome.com/v5.15/icons?d=gallery&p=2&m=free
  dashboardHeader(title = "Test Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Iris", tabName = "iris", icon = icon("tree")),
      menuItem("Cars", tabName = "cars", icon = icon("car")),
      menuItem("Test Schedule", tabName = "schedule", icon = icon("calendar-alt")),
      menuItem("Surveys", tabName = "surveys", icon = icon("poll")),
      menuItem("Mx Data", tabName = "mx_data", icon = icon("wrench"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("iris",
              box(plotOutput("correlation_plot"), width = 8),
              box(
                selectInput("features", "Features:",
                            c("Sepal.Width", "Petal.Length",
                              "Petal.Width")), width = 4
              )
      ),

    tabItem("cars",
              fluidPage(
              h1("Cars"),
              dataTableOutput("carstable")
              )
    ),
    tabItem("schedule",
            fluidPage(
            h1("Test Schedule")
            )
    ),
    tabItem("surveys",
            fluidPage(
            h1("Survey tool")
            )
    ),
    tabItem("mx_data",
            fluidPage(
            h1("Mx data import")
            )
    )
    )
)
)
## ---------------------------------------------------------------------------------------- ##
# This is the meat and potatoes of the app....add more details.
server <- function(input, output) {
 output$correlation_plot <- renderPlot({
   plot(iris$Sepal.Length, iris[[input$features]],
   xlab = "Sepal length", ylab="Features")
 })
 output$carstable <- renderDataTable(mtcars) #mtcars is like iris, just an online data table for demonstration purposes
}
## ---------------------------------------------------------------------------------------- ##
shinyApp(ui, server) #This negates the need to have a server.R and ui.R file.