# This app will look at how close FL statewide elections are..
# and inspire reflection on how Amendment 4 might affect them.


library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(shinythemes)

# Format data
turnout <- read.csv("florida_turnout.csv")
colnames(turnout)[1] <- "Year"
elections <- read.csv("florida_elections.csv")
colnames(elections)[1] <- "ElectionDate"
voter_reg <- read.csv("florida_voter_reg_Aug_19.csv")
colnames(voter_reg)[1] <- "Year"

## for datatable
formatElections <- elections %>%
    filter((RaceCode=="GOV" | RaceCode=="PRE" | RaceCode=="USS") & PartyName != "Write-In") %>%
    group_by(ElectionDate, RaceCode, CanNameLast, CanNameFirst) %>%
    summarise(VotesReceived = sum(CanVotes))
electionsTable1 <- left_join(formatElections, elections[,c(1,3,5,12:14)], by = c("CanNameLast", "CanNameFirst", "ElectionDate"))
electionsTable <- distinct(electionsTable1[,c(1,2,7,4,8,3,6,5)])
electionsTable$ElectionDate <- relevel(electionsTable$ElectionDate, "11/8/2016")
electionsTable$ElectionDate <- relevel(electionsTable$ElectionDate, "11/6/2018")
electionsTable <- arrange(electionsTable, ElectionDate, OfficeDesc, desc(VotesReceived))
## totals that will be used for computations
electionTotals <- formatElections %>%
    ungroup() %>%
    group_by(ElectionDate, RaceCode) %>%
    summarise(electionTotal = sum(VotesReceived))
electionComp <- electionsTable %>% left_join(electionTotals, by=c("ElectionDate", "RaceCode")) %>% mutate(PercentofVote = round((VotesReceived/electionTotal)*100, digits = 2))
electionsTable <- electionComp[,-9]
turnout2 <- mutate(turnout, Pturnout=round((turnout/voterreg)*100, digits=2))[,c(1,4)]
#margin calculation
temp <- top_n(group_by(ungroup(electionsTable),ElectionDate, RaceCode), 2, PercentofVote)
temp2 <- top_n(group_by(ungroup(electionsTable),ElectionDate, RaceCode), 2, VotesReceived)
margins <- diff(temp$PercentofVote)[c(1,3,5,7,9,11,13)]
rawmargins <- diff(temp2$VotesReceived)[c(1,3,5,7,9,11,13)]

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
        
        # Year selection
        selectInput(inputId = "year",
                    "Year (for registration data):",
                    choices = c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019),
                    selected = 2019),
        
        # election type inclusion in turnout graph
        checkboxInput(inputId = "presidential",
                      label = "Presidential election years",
                      value = TRUE),
        checkboxInput(inputId = "midterm",
                      label = "Midterm election years",
                      value = TRUE),
        
        # Turnout Selection ----------------------------------------------
        sliderInput("turnoutSelect",
                    "Hypothetical percentage of turnout in 2020:",
                    min = 0,
                    max = 100,
                    value = 75,
                    step = 1),
        
        #Second Menu Item
        menuItem("Elections Table", icon = icon("table"), tabName = "table")
    )
)

# Dashboard body
body <- dashboardBody(tabItems(
    
    # Plot page
    tabItem(tabName = "plot",
            # info box
            fluidPage( 
                box(title = "Amendment 4", "In November 2018, Florida voters passed a ballot measure to return the right to vote to former felons having met certain criteria. This measure launched one of the largest enfranchisement efforts in modern U.S. history, with more than 1 million people potentially eligible to regain their right to vote. How might this affect future elections? Let's look at the statewide elections between 2012 and 2018.", width = 12)
            ),
            # value boxes
            fluidRow(
                valueBoxOutput("meanMargin"),
                valueBoxOutput("medianMargin"),
                valueBoxOutput("rawMargin")
            ),
            
            # Plot ----------------------------------------------
            fluidRow(
                tabBox(title = "",
                       width = 12,
                       tabPanel("Voter Registration", "Choose a year to see how total party registration changed that year", plotlyOutput("plot_totalreg")),
                       tabPanel("Turnout", plotlyOutput("plot_turnout"), "More than half of Floridians are registered to vote. If half of these newly eligible voters (~500,000) are registered by 2020, and they have the same proportion of party preferences as the general population in 2018, how many votes will be added to the election? (Use slider to adjust hypothetical turnout)", plotlyOutput("plot_hypothetical")))
            )
    ),
    
    # Data Table Page ----------------------------------------------
    tabItem("table",
            fluidPage(
                box(title = "Election Results", DT::dataTableOutput("table"), width = 12))
    
)))

ui <- dashboardPage(header, sidebar, body, skin="purple")

# Define server function required to create plots and value boxes
server <- function(input, output) {
    
    # Reactive registration data ----------------------------------------------
    regInput <- reactive({
        yearreg <- filter(voter_reg, Year == input$year)
        # return dataframe
        return(yearreg)
    })
    
    # Reactive turnout data ----------------------------------------------
    turnInput <- reactive({
        pyears <- turnout2[c(1,3,5),]
        myears <- turnout2[c(2,4,6),]
        M <- c(input$midterm, input$midterm)
        P <- c(input$presidential, input$presidential)
        selectyears <- ifelse((M&P), turnout2, ifelse(M, myears, pyears))
        seriesdata <- data.frame(selectyears)
        colnames(seriesdata) <- c("Year", "Pturnout" )
        # return dataframe
        return(seriesdata)
    })

    # Reactive elections data ----------------------------------------------
    electionsInput <- reactive({
        results <- electionsTable
        # return dataframe
        return(results)
    })
    
    # A plot showing the total party registration -----------------------------
    output$plot_totalreg <- renderPlotly({
        yearreg <- regInput()
        partyreg <- gather(yearreg[1,4:7]/1000000, key="party", value="registered")
        partyreg[,1] <- c("Republicans", "Democrats", "Minor Party", "No Party Preference")
        partyreg$party <- factor(partyreg$party, levels = partyreg$party)

        # Generate Plot ----------------------------------------------
        ggplot(data = partyreg, aes(x = party, y = registered)) + geom_col(fill=c("red", "blue", "pink", "gray")) + labs(y="number of registered voters, in millions") + scale_y_continuous(breaks=1:5) + theme_bw()
    })
    
    # A plot showing how many votes might be added to the 2020 election  -----------------------------
    output$plot_hypothetical <- renderPlotly({
        eligibles <- 500000*c(0.3521515,0.3710195,0.008725483,0.2681035)
        y <- round(eligibles*(input$turnoutSelect/100))
        newvotes <- data.frame(Party = c("Republicans", "Democrats", "Minor Party", "No Party Preference"), added = y/1000, stringsAsFactors = F)
        newvotes$Party <- factor(newvotes$Party, levels = newvotes$Party)
        
        # Generate Plot ----------------------------------------------
        ggplot(data = newvotes, aes(x = Party, y = added)) + geom_col(fill=c("red", "blue", "pink", "gray")) + labs(y="number of added votes, in thousands") + scale_y_continuous(limits=c(0,200)) + theme_bw()
    })
    
    # A plot showing turnout over time -----------------------------
    output$plot_turnout <- renderPlotly({
        seriesdata <- turnInput()

        # Generate Plot ----------------------------------------------
        ggplot(data = seriesdata, aes(x = Year, y = Pturnout)) + geom_line() + labs(y="Turnout, % of registered voters") + theme_bw() + scale_x_continuous(limits = c(2008,2018), breaks = c(2008,2010,2012,2014,2016,2018)) + scale_y_continuous(limits = c(0,100))
    })
    
    # average margin value box ----------------------------------------------
    output$meanMargin <- renderValueBox({
        meanmargin <- round(mean(margins)*-1, digits = 2)
        
        valueBox(subtitle = "Avg Win Margin", value = meanmargin, icon = icon("percent"), color = "green")
    })
    
    # median margin value box ----------------------------------------------
    output$medianMargin <- renderValueBox({
        medianmargin <- median(margins)*-1
        
        valueBox(subtitle = "Median Win Margin", value = medianmargin, icon = icon("percent"), color = "green")
    })
    
    # raw margin value box ----------------------------------------------
    output$rawMargin <- renderValueBox({
        meanrawmargin <- format(round(mean(rawmargins)*-1), big.mark = ",")
        
        valueBox(subtitle = "Avg raw # of votes won by", value = meanrawmargin, icon = icon("people-booth"), color = "orange")
    })
    
    # Data table of characters ----------------------------------------------
    output$table <- DT::renderDataTable(
        electionsInput(), options = list(pageLength=15)
    )
    
    # Context info box ----------------------------------------------
    output$context <- renderInfoBox({
        infoBox(title = "", value = "Amendment 4", subtitle = "In November 2018, Florida voters passed a ballot measure to return the right to vote to former felons having met certain criteria. This measure launched one of the largest enfranchisement efforts in modern U.S. history, with more than 1 million people potentially eligible to regain their right to vote in a state with a population of 21 million.", icon = icon("vote-yea"), color = "blue", width = 12)
    })
    

}

# Run the application 
shinyApp(ui = ui, server = server)
