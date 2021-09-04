#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

## Libraries
library(shiny)
library(ggplot2)
library(tidyverse)
library(plotly)
library(dplyr)
library(leaflet)
library(Rcpp)
library(mapdeck)
library(sfheaders)
library(sf)



## Data Download

# df <- read.csv("E:/Data/Afghanistan/conflict-data-for-afghanistan-2-new.csv")
# saveRDS(df,"df_afghanistan")
df <- readRDS("df_afghanistan")
TOKEN <- "pk.eyJ1IjoieXVzZWk1MjciLCJhIjoiY2tzeDlhdWxoMjZ1YzJ3bzM5d2U4cmoyNSJ9.G1ydpT9ExJcR5gSnxFv46Q"

# data processing
df_temp <- df %>% 
    group_by(year,where_coordinates) %>% 
    summarize(deaths_a=sum(deaths_a),
              deaths_b=sum(deaths_b),
              deaths_civilians=sum(deaths_civilians),
              deaths_unknown=sum(deaths_unknown),
              latitude=latitude[1],
              longitude=longitude[1])
df_temp$deaths_total <- with(df_temp, deaths_a+deaths_b+deaths_civilians,deaths_unknown)
# df_temp <- df_temp %>% filter(where_coordinates!="Afghanistan")

sf <- sf_point(df_temp, x="longitude",y="latitude")
sf$year <- df_temp$year
sf$where_coordinates <- df_temp$where_coordinates
sf$deaths_a <- df_temp$deaths_a
sf$deaths_b <- df_temp$deaths_b
sf$deaths_unknown <- df_temp$deaths_unknown
sf$deaths_civilians <- df_temp$deaths_civilians
sf$deaths_total <- df_temp$deaths_total
sf$popup <- paste(df_temp$where_coordinates,"Deaths A:",df_temp$deaths_a,"Deaths B:",df_temp$deaths_b,"Deaths Civilians:",df_temp$deaths_civilians,"Unknown Deaths:",df_temp$deaths_unknown)
saveRDS(sf,"sf_afghanistan")
sf <- readRDS("sf_afghanistan")



df_actor <- df %>% select(conflict_name,date_start,date_end,side_a,side_b,deaths_a,deaths_b,deaths_civilians,deaths_unknown,where_coordinates,source_article,year)
# df_actor <- df_actor %>% filter(where_coordinates!="Afghanistan")

LOCATION <- c(mean(unique(df$longitude)),mean(unique(df$latitude)))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Conflicts in Afghanistan"),
    
    fluidRow(
        sidebarLayout(
            sidebarPanel(
                sliderInput("year_id",
                            "Year:",
                            min = 1989,
                            max = 2018 ,
                            value = 2018),
                sliderInput("elev_id",
                            "Changing Elevation Level:",
                            min = 1,
                            max = 500,
                            value = 100),
                selectInput("map_id",
                            "Map Type",
                            choices = c("streets",
                                        "outdoors",
                                        "light",
                                        "dark",
                                        "satellite",
                                        "satellite-streets"))
            ),
    
            # Show a plot of the generated distribution
            mainPanel(
               mapdeckOutput("mapdeck")
            )
        
        ),
    ),
    fluidRow(
        sidebarLayout(
            sidebarPanel(
                tabsetPanel(id = "tab_id",
                    tabPanel("Conflict Name",
                             sliderInput("year_id_conf","Year:",
                                         min = 1989,
                                         max = 2018 ,
                                         value = 2018),
                             selectInput("conf_id", "Name of Conflicts:",unique(df$conflict_name)),
                    ),
                    tabPanel("Actors","Take a closer look at the data by actors involeved in conflicts.",
                             sliderInput("year_id_actor",
                                         "Year:",
                                         min = 1989,
                                         max = 2018 ,
                                         value = 2018),
                             checkboxGroupInput("actors_id","Select Actors Involved:", 
                                                unique(c(unique(df$side_a),unique(df$side_b))))
                            # actionButton(inputId = "actors_button",label = "OK")
                            ),
                    tabPanel("District",
                             sliderInput("year_id_dist","Year:",
                                         min = 1989,
                                         max = 2018 ,
                                         value = 2018),
                             textInput("dist_id",
                                       "Specific District:",value = "Kabul city"))
                ),
                
            ),
            
            mainPanel(
                dataTableOutput("table")
            )
        ),
        
    ),
)


server <- function(input, output) {
    
    toListen <- reactive({
        list(input$year_id,input$map_id,input$elev_id,
             input$year_id_conf,input$conf_id,
             input$actors_id,input$year_id_actor,#input$actors_button,
             input$year_id_dist,input$dist_id)
    })
    
    
    output$mapdeck <- renderMapdeck({
        mapdeck(token = TOKEN,
                pitch = 1000,
                location = c(67.45985,34.13859),
                zoom = 4.5)
    })
    
    observeEvent(toListen(),{
        if(input$year_id!=2012){
            mapdeck_update(map_id = "mapdeck") %>% 
                update_style(map = ., style = mapdeck_style(input$map_id)) %>% 
                add_column(data = sf[sf$year==input$year_id,],
                           lat = "latitude",
                           lon = "longitude",
                           elevation = "deaths_total",
                           elevation_scale = input$elev_id,
                           fill_colour = "deaths_total",
                           palette = "diverge_hsv",
                           radius = 5000,
                           tooltip = "popup",
                           auto_highlight = T,
                           legend = TRUE,
                           update_view = F)
        }
        
    })
    
    conf <- reactive({
        input$conf_id
        input$year_id_conf
        df_actor %>% 
            filter(year==input$year_id_conf, conflict_name==input$conf_id) %>% 
          rename( "Conflict Name"="conflict_name", "Starting Date"="date_start", "Ending Date"="date_end", "Side A"="side_a","Side B"="side_b","Deaths A"="deaths_a","Deaths B"="deaths_b","Deaths Civilians"="deaths_civilians", "Deaths Unknown"="deaths_unknown", "Place"="where_coordinates","Source Article"="source_article", "Year"="year")
    })
    ?colnames
    act <- reactive({
        # input$actors_button
        df_actor %>% 
            filter(year==input$year_id_actor,side_a %in% input$actors_id|side_b %in% input$actors_id) %>% 
        rename( "Conflict Name"="conflict_name", "Starting Date"="date_start", "Ending Date"="date_end", "Side A"="side_a","Side B"="side_b","Deaths A"="deaths_a","Deaths B"="deaths_b","Deaths Civilians"="deaths_civilians", "Deaths Unknown"="deaths_unknown", "Place"="where_coordinates","Source Article"="source_article", "Year"="year")
      })
    
    district <- reactive({
        input$dist_id
        df_actor %>%
            filter(year==input$year_id_dist,
                   where_coordinates==input$dist_id) %>% 
          rename( "Conflict Name"="conflict_name", "Starting Date"="date_start", "Ending Date"="date_end", "Side A"="side_a","Side B"="side_b","Deaths A"="deaths_a","Deaths B"="deaths_b","Deaths Civilians"="deaths_civilians", "Deaths Unknown"="deaths_unknown", "Place"="where_coordinates","Source Article"="source_article", "Year"="year")
    })
    names(df_actor)
    output$table <- renderDataTable(
        switch(input$tab_id,
               "Conflict Name"= conf(),
               "Actors"= act(),
               "District"= district()
               ),
        

        )
}

# Run the application 
shinyApp(ui = ui, server = server)
