#TidyTuesday
#Female earnings
#Shiny, first try

#Load libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(gganimate)
library(extrafont)
library(readr)
library(shiny)
library(rsconnect)

#Read file
female <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-05/employed_gender.csv")

#Subset of file
female2 <- subset(female, year %in% c(1970, 1980, 1990, 2000, 2010))

#Make data.matrix
names(female2)[1] <- ""
female2 <-data.matrix(female2)

#Make variabele year -> rownames
female3 <- female2[,-1]
rownames(female3) <- female2[,1]

#Ui
ui <- fluidPage(
    titlePanel('TidyTuesday:
               Employment through the years for females and males'),
    sidebarLayout(
      sidebarPanel(
      selectInput("region", "Type of employment by gender",
                  choices=colnames(female3)),
      hr(),
      helpText("Employment from 1970 to 2010, 
               Source: Bureau of Labor")),
  mainPanel(
    plotOutput("EmployeePlot")
  )
)
)

#Server
server <- function(input, output) {
  
#Fill in the spot we created for the barplot
output$EmployeePlot <- renderPlot ({
    
#Barplot
barplot(female3[,input$region], 
            main="",
            col = "darkmagenta", border = "black",
            ylab="% employed",
            xlab="year")
  })
}

#Run it
shinyApp(ui, server)








