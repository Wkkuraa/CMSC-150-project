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
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
  titlePanel("Luzon Grid Dispatch and Price Simulator"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("selectedgroups", "Plant groups", choices = plantgroups, selected = plantgroups),
      checkboxInput("CO2ON", "Apply CO2 cap", FALSE),
      conditionalPanel(
        condition = "input.CO2ON == true",
        numericInput("CO2LIMIT", "CO2 limit", value = 165000, min = 0)
      ),
      actionButton("run", "Solve")
    ),
    mainPanel(
      textOutput("msg"),
      h3("Number Of Iterations"),  textOutput("Iter"),
      h3("Minimum total cost"),  textOutput("z"),
      h3("Dispatch x (MW)"),     tableOutput("sol"),
      h3("Price (P/MWh)"),       tableOutput("price"),
    )
  )
)
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  result <- eventReactive(input$run, {
    validate(need(length(input$selectedgroups) > 0, "Please Select at least One Plant Group"))
    
    initmatrix<- build_matrix(primal_table,input$selectedgroups,input$CO2ON,input$CO2LIMIT)
    results <- Simplex(initmatrix,input$selectedgroups)
    return(results) 
  })
  
  output$msg <- renderText({
    r <- result()
    if(is.nan(r$Z) == TRUE){
      paste("Problem is infeasible")
    }else if(is.nan(r$Z) == FALSE){
      paste("Optimal Solution Found")
    }
  })
  
  output$z <- renderText({
    r <- result()
    paste("Minimum Cost of Operating Selected Groups: ₱",r$formatZ)
    })
  
  output$Iter <- renderText({
    r<-result()
    if(is.nan(r$Z) == TRUE){
      paste(r$iter)
    }else if(is.nan(r$Z) == FALSE){
      paste(r$iter)
    }
  })
  
  output$sol <- renderTable({
    r <- result()
    r$basicSolution}, rownames = TRUE)
  
  output$price <- renderTable({
    r <- result()
    t(r$shadowprice)}, rownames = FALSE, colnames = TRUE)
}
# Run the application 
shinyApp(ui = ui, server = server)
