#Homework 1

library(shiny)
library(ggplot2)
library(datasets)
library(reshape2)
library(dplyr)
library(plotly)
library(DT)

#Rename Data
arrests <- USArrests


#Delete percentage of population living in Urban Areas
arrests$UrbanPop <- NULL

#Create new variable with state names
arrests$State <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas" ,"Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota","Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")

#Get dataset for shiny table
PlotArrests <- data.frame(arrests)
PlotArrests$State <- NULL

#Subset data top 5 most populated states
MostPop <- subset (arrests, subset = 
                     (arrests$State == "California") |
                     (arrests$State == "Texas") |
                     (arrests$State == "Florida") |
                     (arrests$State == "New York") |
                     (arrests$State == "Pennsylvania")
)

#Melt data to put it in long form so I can use it to fill by variable in ggplot
USarrests <- melt(MostPop, id.vars = "State")

#Capitalize column names because it's bothering me not to do that.
names(USarrests)[2]<-"Variable"
names(USarrests)[3]<-"Value"

pdf(NULL)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel("USA Arrests in 1973 Grid"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Explore Arrests by Crime in the 5 Most Populous States"),
      
      selectInput("Var",
                  "Crime:",
                  c("Murder", 
                    "Assault",
                    "Rape"),
                  selected = "Murder")
),
    
    mainPanel(
      textOutput("selected_var"),
      
      fluidRow(column(3,
                      fileInput("file", h3("File input")))),
      mainPanel(
        plotlyOutput("plot")
),
      fluidRow(column(12,
                      DT::dataTableOutput("table")
)
))))

server <- function(input, output) {
  
  output$plot <- renderPlotly({
    ggplot(data = USarrests, aes(x = State, y = Value, fill = Variable)) + geom_bar(stat = "identity")
  })  
  output$table <- DT::renderDataTable({
    (PlotArrests)
  })
}

shinyApp(ui, server)
