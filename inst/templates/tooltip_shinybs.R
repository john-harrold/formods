library(shiny)
library(shinydashboard)
library(shinyBS)

ui <- dashboardPage(
  dashboardHeader(title = "Tooltips &\n Popovers"),
  dashboardSidebar(collapsed = TRUE),
  dashboardBody(
    fluidRow(
      h4("The slider input uses a UI Tooltip - on hover, the chart uses a SERVER popover - on click"),
      box(
        sliderInput("bins1",
                    "Number of bins:",
                    min = 1,
                    max = 50,
                    value = 30),
        bsTooltip("bins1", "The wait times will be broken into this many equally spaced bins",
                  "right", options = list(container = "body"))
      ),
      box(
        plotOutput("distPlot1")
      )
    ),
    fluidRow(
      h4("The slider input uses a SERVER Tooltip - on hover, the chart uses a UI popover - on hover"),
      box(
        sliderInput("bins2",
                    "Number of bins:",
                    min = 1,
                    max = 50,
                    value = 30)
      ),
      box(
        plotOutput("distPlot2"),
        bsPopover("distPlot2", "Data", content = "This is text") # default is hover
      )
    )
  )
)

server <- function(input, output, session) {
  output$distPlot1 <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins1 + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  })
  addPopover(session, "distPlot1", "Data", content = "This is text", trigger = 'click')
  
  output$distPlot2 <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins2 + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  })
  addTooltip(session, "bins2", "This is text", "right", options = list(container = "body"))
}

shinyApp(ui, server)