#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(shiny)
library(bslib)
source("project.R",local=TRUE)
plantgroups <- c("Coal", "Nat.Gas", "Geo", "Hydro", "Oil", "Solar", "Wind")

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Luzon Grid Dispatch and Price Simulator"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("groups", "Plant groups", choices = plantgroups, selected = plantgroups),
      checkboxInput("co2on", "Apply CO2 cap", FALSE),
      numericInput("co2lim", "CO2 limit", value = 180000, min = 0),
      actionButton("run", "Solve")
    ),
    mainPanel(
      textOutput("msg"),
      h4("Minimum total cost"),  textOutput("z"),
      h4("Dispatch x (MW)"),     tableOutput("sol"),
      h4("Price (P/MWh)"),       tableOutput("price"),
      h4("Final tableau"),       tableOutput("tab")
    )
  )
)
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  result <- eventReactive(input$run, {
    list(groups = input$groups, co2on = input$co2on, co2lim = input$co2lim)
  })
  
  output$msg <- renderText({
    r <- result()
    paste0("Solve clicked. Groups: ", paste(r$groups, collapse = ", "),
           " | CO2 cap: ", if (r$co2on) paste("ON at", r$co2lim) else "OFF")
  })
  
  output$z <- renderText("(placeholder) cost will appear here")
  
  output$sol <- renderTable({
    r <- result()
    matrix(0, nrow = length(r$groups), ncol = 6,
           dimnames = list(r$groups, paste0("B", 1:6)))
  }, rownames = TRUE)
  
  output$price <- renderTable({
    result()
    t(setNames(rep(0, 6), paste0("B", 1:6)))
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
