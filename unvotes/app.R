library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(plotly)

# downloading data
tuesdata <- tidytuesdayR::tt_load('2021-03-23')
unvotes <- tuesdata$unvotes
temp_issue <- tuesdata$issues %>% select(-short_name)

# combining data frames
unvotes_temp <- unvotes %>% 
    left_join(temp_issue, by="rcid") %>% 
    left_join(roll_calls, by="rcid") %>% 
    group_by(country,date) %>% 
    mutate(yes_rate=mean(vote=="yes")) %>% 
    ungroup()
unvotes_temp <- unvotes_temp %>% 
    mutate(issue = ifelse(is.na(issue), "Not Categorized", issue))

# country name
country <- unique(unvotes$country)
date <- unique(unvotes_temp$date)
issue <- unique(unvotes_temp$issue)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Exploring UN Votes"),
    fluidRow(
        # Sidebar with a slider input for number of bins 
        sidebarLayout(
            sidebarPanel(
                selectInput("country_id", label = "Country",
                            choices = country %>% 
                                as.list()
                            ),
                ),
    
            # Show a plot of the generated distribution
            mainPanel(
               plotlyOutput(outputId = "Plot")
            )
        )
        
    ),
    
    # country, year -> details of the description
    fluidRow(
        sidebarLayout(
            sidebarPanel(
                
                # select year
                selectInput("date_id", label = "Date",
                            choices = date %>% 
                                as.character.Date() %>% 
                                as.list()
                ),
                
                # select issue
                selectInput("issue_id", label = "Issue",
                            choices = issue %>% 
                                as.list()
                ),
            ),
            
            # Show a plot of the generated distribution
            mainPanel(
                # textOutput("text",inline = FALSE)
                tableOutput("table")
                
            )
        )
        
    )

)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$Plot <- renderPlotly({
        
        # make ggplot and convert it to plotly
        ggfig <- unvotes_temp %>%
            filter(country==input$country_id) %>%
            ggplot(aes(date,yes_rate,color=issue))+
            geom_point()+
            geom_smooth()
        ggplotly(ggfig)
    })
    # output$text <- renderText({
    #     temp_rcid <- unvotes_temp %>% 
    #         filter(date==input$date_id) %>% 
    #         pull(rcid)
    #     tuesdata$roll_calls %>% 
    #         filter(rcid==temp_rcid) %>% 
    #         pull(descr)
    # 
    # })
    output$table <- renderTable({
        unvotes_temp %>% 
            filter(date==input$date_id, issue==input$issue_id) %>% 
            select(country, vote, descr)
        # tuesdata$roll_calls %>% 
        #     filter(rcid==temp_rcid) %>% 
        #     select(descr) %>% as.data.frame()
        
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
