# library(shiny)
# ui <- fluidPage(textInput("a","")) 
# 
# server <- function(input,output){  
#   output$b <-renderText({ input$a })
#   }
# 
# shinyApp(ui, server)

# 
# library(shiny)
# 
# ui <- fluidPage(textInput("a",""), 
#                 textOutput("b") )
# 
# server <- function(input,output){  
#   output$b <-  renderText({
#     isolate({input$a})
#     })
#   }
# shinyApp(ui, server)



library(shiny)
ui <- fluidPage(textInput("a",""),  
                textInput("b", ""),
                h3(textOutput('c'), align = 'center')
                ) 

server <- function(input,output){
  re <- reactive({  
    paste(input$a,input$b)
          })
    
    output$c <- renderText({
      re()  
    }) 
}


shinyApp(ui, server)




