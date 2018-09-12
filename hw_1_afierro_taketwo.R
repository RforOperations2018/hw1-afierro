#Homework 1
#Allyson Fierro

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
# In the future would be a great reason to use the dplyr function add_rownames() but this works too, no deduction, just a tip!
arrests <- arrests %>%
  tibble::rownames_to_column(var = "State")

#Get dataset for shiny table
PlotArrests <- data.frame(arrests)
PlotArrests$State <- NULL

#Subset data top 5 most populated states
MostPop <- subset (arrests, State %in% c("California", "Texas", "Florida", "New York", "Pennsylvania"))

#Melt data to put it in long form so I can use it to fill by variable in ggplot
USarrests <- melt(MostPop, id.vars = "State")

#Capitalize column names because it's bothering me not to do that.
# This bugs me too! You can use toTitleCase from the tools package in the future
names(USarrests) <- tools::toTitleCase(names(USarrests))

pdf(NULL)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("USA Arrests in 1973 Grid"),
  inputPanel(
      helpText("Explore Arrests by Crime in the 5 Most Populous States"),
      
      selectInput("Var",
                  "Crime:",
                  c("Murder", 
                    "Assault",
                    "Rape"),
                  selected = "Murder"),
      fileInput("file", h3("File input"))
      ),
    fluidPage(
      # You don't have anything in the server function creating this output
      textOutput("selected_var"),
      fluidRow(
        plotlyOutput("plot")
        ),
      fluidRow(column(12,
                      DT::dataTableOutput("table")
                      )
               )
      )
)

server <- function(input, output) {
  # Rendering Text
  output$selected_var <- renderText({
    paste("You have selected the following variables:", input$Var)
  })
  output$plot <- renderPlotly({
    ggplot(data = USarrests, aes(x = State, y = Value, fill = Variable)) + geom_bar(stat = "identity")
  })  
  output$table <- DT::renderDataTable({
    (PlotArrests)
  })
}

shinyApp(ui, server)
