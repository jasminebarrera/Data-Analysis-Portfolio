library(shiny)
library(markdown)
library(MASS)

setwd("C:\\Users\\jasmi\\Desktop\\JOB\\Portfolio\\Framingham_Heart\\FramShinyApp")
fhs <- read.csv("fhs-data.csv", stringsAsFactors = FALSE)
print(str(fhs))

# Images
b64 <- base64enc::dataURI(file="C:\\Users\\jasmi\\Desktop\\JOB\\Portfolio\\Framingham_Heart\\FramShinyApp\\www\\introTable.PNG", mime="image/png")
barTY <- base64enc::dataURI(file="C:\\Users\\jasmi\\Desktop\\JOB\\Portfolio\\Framingham_Heart\\FramShinyApp\\www\\barTenYear.PNG", mime="image/png")
boxAge <- base64enc::dataURI(file="C:\\Users\\jasmi\\Desktop\\JOB\\Portfolio\\Framingham_Heart\\FramShinyApp\\www\\boxAge.PNG", mime="image/png")
boxSBP <- base64enc::dataURI(file="C:\\Users\\jasmi\\Desktop\\JOB\\Portfolio\\Framingham_Heart\\FramShinyApp\\www\\boxSysBP.PNG", mime="image/png")

fhs_step<-na.omit(fhs)
fhs_step$male <- as.factor(fhs_step$male)
fit.1 <- glm(TenYearCHD~male + age + cigsPerDay + totChol + sysBP + glucose,
             family=binomial(link=logit), data=fhs_step)
model_summary <- summary(fit.1)


ui <- navbarPage("Coronary Heart Disease Predictor",
                 tabPanel("Intro",
                          fluidPage(
                            tags$style(HTML('body {font-family:Georgia}')),
                            titlePanel("Introduction"),
                            fluidRow(
                              column(12, align="center", style="font-size:20px;",
                                     includeMarkdown("intro-text.md"),
                                     div(style="display: inline-block;", img(src=b64, height=600, width=600))
                              )
                            ),
                            fluidRow(
                              column(12, align="center", style="font-size:20px;",
                                     includeMarkdown("eda-text.md"),
                                     div(style="display: inline-block;", img(src=barTY, height=600, width=600)),
                                     div(style="display: inline-block;", img(src=boxAge, height=600, width=600)),
                                     div(style="display: inline-block;", img(src=boxSBP, height=600, width=600))
                              )
                            )
                          )
                 ),
                 tabPanel("Model",
                          fluidPage(
                            tags$style(HTML('body {font-family:Georgia}')),
                            titlePanel("Logistic Regression Model"),
                            fluidRow(
                              column(6, align="left", style="font-size:20px;",
                                     includeMarkdown("model-text.md"),
                                     verbatimTextOutput("model_summary")
                                     )
                            )
                          )
                 ),
                 tabPanel("Apply",
                          fluidPage(
                            tags$style(HTML('body {font-family:Georgia}')),
                            titlePanel("Predict CHD"),
                            sidebarPanel(
                              tags$style(HTML('body {font-family:Georgia}')),
                              style="font-size:20px;",
                                       includeMarkdown("apply-text.md"),
                                       radioButtons("gender", h3("Gender"),
                                                    choices=list("Female"=0, "Male"=1), selected=0),
                                       numericInput("age", h3("Age"), value=1,
                                                    min=1, max=130, width=100),
                                       numericInput("cigs", h3("Number of Cigarettes/Day"),
                                                    value=0, min=1, max=120, width=200),
                                       numericInput("chol", h3("Total Cholestrol (mg/dL)"),
                                                    value=1, min=1, max=400, step=1, width=200),
                                       numericInput("sbp", h3("Systolic Blood Pressure (mm Hg)"),
                                                    value=1, min=1, max=200, step=1, width=250),
                                       numericInput("gluc", h3("Glucose Level (mm/dL)"),
                                                    value=1, min=1, max=500, step=1, width=250),
                              actionButton("button", "Submit")
                                       ),
                            mainPanel(
                              tags$style(HTML('body {font-family:Georgia}')),
                              style="font-size:50px;",
                              h1("Ten Year CHD?"),
                              verbatimTextOutput("value")
                              )
                            )
                          )
                 )


server <- function(input, output) {
  
  output$text <- renderText({ input$txt })
  output$model_summary <- renderPrint({
    model_summary
  })
  value <- eventReactive(input$button, {
    data <- ({data.frame(male=input$gender,
                 age=input$age,
                 cigsPerDay=input$cigs,
                 totChol=input$chol,
                 sysBP=input$sbp,
                 glucose=input$gluc)
      })
  })
  pred <- reactive({
    predict(fit.1, value(), type="response")
  })
  output$value <- renderPrint(
    ifelse(as.vector(pred()) > 0.5, "Yes", "No"), width=100
    )
}





shinyApp(ui = ui, server = server)


