# This app will look at how close FL statewide elections are..
# and inspire reflection on how Amendment 4 might affect them.


library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(ggplot2)
library(shinythemes)

# Format data
turnout <- read.csv("florida_turnout.csv")
colnames(turnout)[1] <- "Year"
elections <- read.csv("florida_elections.csv")
colnames(elections)[1] <- "ElectionDate"
voter_reg <- read.csv("florida_voter_reg_Aug_19.csv")

## for datatable
formatElections <- elections %>%
    filter((RaceCode=="GOV" | RaceCode=="PRE" | RaceCode=="USS") & PartyName != "Write-In") %>%
    group_by(ElectionDate, RaceCode, CanNameLast, CanNameFirst) %>%
    summarise(VotesReceived = sum(CanVotes))
electionsTable1 <- left_join(formatElections, elections[,c(1,3,5,12:14)], by = c("CanNameLast", "CanNameFirst", "ElectionDate"))
electionsTable <- distinct(electionsTable1[,c(1,7,4,8,3,6,5)])
electionsTable$ElectionDate <- relevel(electionsTable$ElectionDate, "11/8/2016")
electionsTable$ElectionDate <- relevel(electionsTable$ElectionDate, "11/6/2018")
electionsTable <- arrange(electionsTable, ElectionDate, OfficeDesc, desc(VotesReceived))
## totals that will be used for later computations
electionTotals <- formatElections %>%
    ungroup() %>%
    group_by(ElectionDate, RaceCode) %>%
    summarise(electionTotal = sum(VotesReceived))

# Header and title
header <- dashboardHeader(title = "Florida Statewide Elections",
                          titleWidth = 280
                          
)

# Dashboard Sidebar
sidebar <- dashboardSidebar(
    sidebarMenu(
        id = "tabs",
        
        # First Menu Item
        menuItem("Charts", icon = icon("bar-chart"), tabName = "plot"),
        
        # Inputs to plots
        
        #Second Menu Item
        menuItem("Table", icon = icon("table"), tabName = "table")
    )
)

# Dashboard body
body <- dashboardBody(tabItems(
    
    # Plot page
    tabItem(tabName = "plot",
            # info box
            fluidPage( 
                box(title = "Amendment 4", "In November 2018, Florida voters passed a ballot measure to return the right to vote to former felons having met certain criteria. This measure launched one of the largest enfranchisement efforts in modern U.S. history, with more than 1 million people potentially eligible to regain their right to vote in a state with a population of 21 million.", width = 12)
            ),
            # value boxes
            fluidRow(
                valueBoxOutput("pMargins"),
                valueBoxOutput("rawMargins")
            )
            
            #
    ),
    
    # Data Table Page ----------------------------------------------
    tabItem("table",
            fluidPage(
                box(title = "Election Results", DT::dataTableOutput("table"), width = 12))
    
)))

ui <- dashboardPage(header, sidebar, body, skin="purple")

# Define server function required to create plots and value boxes
server <- function(input, output) {

    # Data table of characters ----------------------------------------------
    output$table <- DT::renderDataTable(
        electionsTable, options = list(pageLength=15)
    )
    
    # Context info box ----------------------------------------------
    output$context <- renderInfoBox({
        infoBox(title = "", value = "Amendment 4", subtitle = "In November 2018, Florida voters passed a ballot measure to return the right to vote to former felons having met certain criteria. This measure launched one of the largest enfranchisement efforts in modern U.S. history, with more than 1 million people potentially eligible to regain their right to vote in a state with a population of 21 million.", icon = icon("vote-yea"), color = "blue", width = 12)
    })
    

}

# Run the application 
shinyApp(ui = ui, server = server)
