# Convergence of probability estimate

# Input: Number of Coin toss
# Output: Probability estimate plot

# Defining user interface
library(shiny)

ui <- shinyUI(
  fluidPage(
    titlePanel("Convergence assumption checking"), # This will be the title of the application
    sidebarLayout(
      sidebarPanel(
#       selectInput("n", "Sample Size", c(10,50,100,1000, 10000),selected = 10),
        sliderInput("n", "Number of Experiment", min = 2, max = 10000, value = 2)
      ),
      mainPanel(
        plotOutput("plot1")
      )
    )
  )
)

server <- shinyServer(
    function(input,output){
      
      toss <- reactive({
      #sumX <- sum(rbinom(input$n,1,.5))
      set.seed(1234)
      prob <- do.call(rbind, 
                      lapply(2:input$n, function(i) c(i, mean(rbinom(i,1,.5))))
                      )
      prob
    })
    output$plot1 <- renderPlot({
      plot(toss()[,1], toss()[,2], type = 'l', xlab = "Number of Experiment", 
           ylab= "Probability",
           main = "Probablity Estimate \n Number of experiment varies over 2:100K",
           ylim = c(0,1))
      abline(h=.5, col='red')

    })
  }
)

shinyApp(ui,server)
