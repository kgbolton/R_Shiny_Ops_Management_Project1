# This app will look at how close FL statewide elections are..
# and inspire reflection on how Amendment 4 might affect them.


library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(ggplot2)
library(shinythemes)

# Format data

# Header and title
header <- dashboardHeader(title = "Florida Statewide Elections",
                          #stuff)
                          
)

# Dashboard Sidebar
sidebar <- dashboardSidebar(
    sidebarMenu(
        id = "tabs",
    )
)

# Dashboard body
body <- dashboardBody(tabItems(
    
    # stuff
))

ui <- dashboardPage(header, sidebar, body)

# Define server function required to create plots and value boxes
server <- function(input, output) {

    #stuff
}

# Run the application 
shinyApp(ui = ui, server = server)
