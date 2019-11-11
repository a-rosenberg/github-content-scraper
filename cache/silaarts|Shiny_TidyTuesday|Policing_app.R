#TidyTuesday
#Policing

#Load libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(gganimate)
library(extrafont)
library(readr)
library(shiny)
library(rsconnect)
library(RColorBrewer)
library(DT)

#Read file
combined_data <- readr::read_csv("https://raw.githubusercontent.com/5harad/openpolicing/master/results/data_for_figures/combined_data.csv")

#Delete columns
data <- select(combined_data, -c(7, 9:11))

#ui
ui <- 
  fluidPage(
    plotOutput("plot", click = "plot_click"),
    hr(),
    helpText("Source: Stanford Policing Project | Plot by @sil_aarts"),
    fluidRow(
      column(5, 
        selectInput('driver_race',"Driver's race", choices=unique(data$driver_race)),
        selectInput('state', "State", choices =unique(data$state)),
      column(8,
             h4("Click the points for info!"),
             dataTableOutput("click_info"))
      )
)
)

#server
server <- function(input, output) {
  
#Make scatterplot
 output$plot <- renderPlot({
        data <- data[data$driver_race == input$driver_race, ]
        data <- data[data$state == input$state, ]
        ggplot(data) +
          geom_point(aes(x = search_rate, y = arrest_rate, color=input$state, shape=input$driver_race)) +
          labs(x = "search rate %", 
               y = "arrest rate %", 
               title = "TidyTueday: Search rate & arrest rate")+
          theme(legend.background = element_rect(fill="white", size=0.5, linetype="solid", colour="black"),
                panel.background = element_rect(fill = "white"),
                legend.position = "none",
                panel.border = element_rect(colour = "black", fill=NA, size=0.5),
                panel.grid.major.x =element_blank(),
                panel.grid.major.y =element_blank(),
                panel.grid.minor.x =element_blank(),
                panel.grid.minor.y =element_blank(),
                axis.text.x= element_text(size=10, hjust=1, face='bold', colour='black'),
                axis.text.y= element_text(size=10,face='bold', colour='black'),
                axis.title.x = element_text(color = "black", size = 14, angle = 0, hjust = 0.6, vjust = 1, face = "bold"),
                axis.title.y = element_text(color = "black", size = 14, angle = 90, hjust = 0.5, vjust = 1, face = "bold"))
    })

#Make Table
output$click_info <- 
  DT::renderDataTable(DT::datatable({
    nearPoints(data, input$plot_click)},

#Change lay-out table
options = list(pageLength = 5,dom = 'ftl', searching=FALSE), rownames = FALSE, escape = FALSE) %>%   
  formatStyle('state', color = 'white',backgroundColor = 'grey', fontWeight = 'bold'))
}


shinyApp(ui=ui, server=server)


