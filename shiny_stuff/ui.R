library(shiny)
shinyUI(pageWithSidebar(
  headerPanel('Finish My Thought'),
  sidebarPanel(
    textInput(inputId="phrase", label = NULL, 
              value= NULL,
              placeholder = "Enter a word or phrase ..."),
    br(),
    actionButton(inputId = "go", label = "Predict")
  ),
  mainPanel(
    h4('You entered:'),
    verbatimTextOutput("oid1"),
    img(src="crystal_ball01.png", height = 200, width = 300),
    h4('The next word is . . . '),
    verbatimTextOutput("prediction")
    )
))
