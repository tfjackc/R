library(shiny)

ui <- fluidPage(
  h1("Hello, world!")
)

server <- function(input, output, session) {
}

shinyApp(ui, server)
