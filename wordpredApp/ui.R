shinyUI(fluidPage(
        titlePanel("Word Prediction"),
        
        sidebarLayout(
                sidebarPanel(
                        p("This app implements a word prediction algorithm which predicts the 6 most probable
                          next-words. Type your sentence into the 
                          text input box and click a button to select a desired word. 
                          If no desired word presents, you can also type along in the text box. The result
                          is printed on the panel in the main panel on the lower right.",
                          style = "font-family: 'times'; font-si16pt"),
                        #
                        br(),
                        br(),
                        textInput("sentence", "Type a sentence:")
                ),
        
                mainPanel(
                        img(src = "nlp.png"),#, height = 200, width = 200)
                        uiOutput("uiOutputPanel"),
                        h5(textOutput("sentenceEntered", container=span))
                )
        )
))