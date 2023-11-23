library(shiny)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(shinyjs)

ui <- fluidPage(
    includeCSS("www/custom.css"),
    titlePanel("Iris data"),
    img(src = "pic1.png"),
  sidebarLayout(
    sidebarPanel(
  varSelectInput("xvar", "X variable", iris, selected = "Sepal.Length"),
    varSelectInput("yvar", "Y variable", iris, selected = "Petal.Width"),
    checkboxGroupInput(
      "species", "Filter by species",
      choices = unique(iris$Species), 
      selected = unique(iris$Species)
    ),
    hr(),
    checkboxInput("by_species", "Show species", TRUE),
    checkboxInput("smooth", "Add smoother")),
  mainPanel(plotOutput("scatter"))
))

server <- function(input, output, session) {
  subsetted <- reactive({
    req(input$species)
    iris %>% filter(Species %in% input$species)
  })
  
  output$scatter <- renderPlot({
    ggplot(subsetted(), aes(!!input$xvar, !!input$yvar)) + list(
      theme(legend.position = "bottom"),
      if (input$by_species) aes(color = Species),
      geom_point(),
      if (input$smooth) geom_smooth(se = FALSE, method = lm))
    })
}
shinyApp(ui = ui, server = server)
