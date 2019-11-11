#load libraries ####
library(maps)
library(here)
library(tidyverse)
library(ggthemes)
#https://socviz.co/maps.html


#set up a map of united states ####
us_states <- map_data("state")


# get data ####
data <- read_csv(here("CatDogOwnership_ShinyApp/Data", "cats_vs_dogs.csv"))
data$region <- tolower(data$state)
bigdata <- left_join(us_states, data)



# source helper function ####
source('helper.R')



# User Interface ####
ui <- fluidPage(
  titlePanel("DogCatVis"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Dogs and Cats"),
      
      selectInput("var", 
                  label = "Choose a variable to display",
                  choices = c("Percent Pet Households", "Percent Dog Owners"),
                  selected = "Percent Pet Households")
    ),
      

    mainPanel(plotOutput("map"))
  )
)

# Server logic ####
server <- function(input, output) {
  
  output$map <- renderPlot({
    
      var <- switch(input$var, 
                     "Percent Pet Households" = bigdata$percent_pet_households,
                     "Percent Dog Owners" = bigdata$percent_dog_owners)
      
      color <- switch(input$var, 
                      "Percent Pet Households" = "#CB454A",
                      "Percent Dog Owners" = "#45CB70")

      percent_map(bigdata, var, color)
  })
}

# Run app ----
shinyApp(ui, server)
