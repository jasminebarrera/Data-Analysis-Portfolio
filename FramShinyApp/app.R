library(shiny)
setwd("C:\\Users\\jasmi\\Desktop\\JOB\\Portfolio\\Framingham_Heart\\FramShinyApp")
fhs <- read.csv("fhs-data.csv", stringsAsFactors = FALSE)
print(str(fhs))





ui <- fluidPage(
  navbarPage("Coronary Heart Disease Predictor",
             tabPanel("Intro",
                      p("Introduction", style= "font-size: 50px"),
                      p("The Framingham heart study found on Kaggle.com was used to
                        build a logistic regression model that could predict whether or not
                        a person will develop coronary heart disease (CHD) in 10 years. The 
                        parameters used in the study were as follows:"),
                      p(""),
                      img(src="table1.PNG")
                      ),
             tabPanel("Model"),
             tabPanel("Apply")),
  mainPanel(
    
  )
)

server <- function(input, output) {
  output$text <- renderText({ input$txt })
}

shinyApp(ui = ui, server = server)


