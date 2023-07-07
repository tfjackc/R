library(shiny)
library(ggplot2)

animals <- c("dog", "cat", "mouse", "bird", "other", "I hate animals")

ui <- fluidPage(
  radioButtons("animal", "What's your favourite animal?", animals)
)

server <- function(input, output, session) {

}

shinyApp(ui, server)