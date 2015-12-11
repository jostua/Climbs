require("shiny")
require("dplyr")
require("ggplot2")

ui <- fluidPage(
  "Climb Stats",
  plotOutput("value")
)

server <- function(input, output) {
  output$value <- renderPlot({
    ggplot(climbs_per_session, aes(x = D ate, y = Climbs, fill = Sends/Climbs)) + geom_bar(stat="identity")
  })
}

shinyApp(ui = ui, server = server)