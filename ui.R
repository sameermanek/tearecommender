
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#


# This is the UI file -- it is run on the server once to generate the UI
# It implements bootstrap

library(shiny)
library(shinythemes)

shinyUI(fluidPage(theme = shinytheme('flatly'),

  # Application title
  titlePanel("Tea Recommendation Engine"),

  # Sidebar with inputs
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = 'likedTeas', 
                  label = "Select teas that you like",
                  choices = chr.choices,
                  multiple = TRUE,
                  selectize = TRUE),
      selectInput(inputId = 'dislikedTeas',
                  label = "Select some teas that you dislike",
                  choices = chr.choices,
                  multiple = TRUE,
                  selectize = TRUE)
    ),

    # Show the results
    mainPanel(
      h2("Recommendations"),
      wellPanel(tableOutput("teaRecommendations")),
      hr(),
      h2("Teas you liked"),
      wellPanel(tableOutput("likedTeasDisplay")),
      h2("Teas you disliked"),
      wellPanel(tableOutput("dislikedTeasDisplay"))
      
    )
  )
))
