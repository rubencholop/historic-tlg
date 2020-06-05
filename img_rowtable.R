library(shiny)
library(DT)
ui <- fluidPage(
  tabsetPanel(
    tabPanel("One",
             DT::dataTableOutput("test1")
    ),
    tabPanel("two",
             numericInput("length","Length",0,0,10)
    )))
server <- function(input, output, session) {
  df <- reactive({
    cbind(seq_len(nrow(mtcars)),mtcars)
  })
  output$test1 <- DT::renderDataTable({
    df()
  },rownames=FALSE,options=list(dom="t"),
  callback=JS(
    'table.on("click.dt", "tr", function() {

    tabs = $(".tabbable .nav.nav-tabs li a");
    var data=table.row(this).data();

    document.getElementById("length").value=data[0];
    Shiny.onInputChange("length",data[0]);
    $(tabs[1]).click();
    table.row(this).deselect();})'
  ))
  
}
shinyApp(ui = ui, server = server)


library(shiny)
library(shinydashboard)
library(shinyBS)
library(DT)

ui <- dashboardPage(
  dashboardHeader(title = span(tagList(icon("image"), "Example"))),
  dashboardSidebar(),
  dashboardBody(
    
    div(style="display:inline-block",uiOutput("infoButton")),
    
    DT::dataTableOutput("table2")
    
  )
)
ddResourcePath("Images","D/HDImages") # Images are located outside shiny App

LeafNames <- c('Leaf1.jpg','Leaf2.jpg','Leaf3.jpg','Leaf4.jpg','Leaf5.jpg','Leaf6.jpg','Leaf7.jpg','Leaf8.jpg','Leaf9.jpg','Leaf10.jpg')
LeafTable <- data.frame(LeafNames)
LeafTable<- within(LeafTable, thumbnail <- paste0("<img src='","Images/",LeafTable$LeafNames,"' height='50'></img>"))

server <- function(input, output) { 
  
  output$table2<-DT::renderDataTable({
    
    responseDataFilter2 <- LeafTable[,c(2,1)]
    
    displayableData<-DT::datatable(
      data = as.data.frame(responseDataFilter2, stringAsFactors = FALSE, row.names = NULL),
      escape=FALSE,
      selection="single",
      rownames=FALSE,
      colnames=c(" ","Name"),
      callback = JS("table.on('dblclick.dt', 'td', function() {
                    var row=table.cell(this).index().row;
                    Shiny.onInputChange('rows_home',[row, Math.random()])});
                    table.on('click.dt', 'td', function() {
                    var k=table.cell(this).index().row;
                    if(table.rows('.selected').indexes().toArray()!= '' && table.rows('.selected').indexes().toArray() == k){k=-1;}
                    Shiny.onInputChange('rows_up_home',[k, Math.random()]);
                    Shiny.onInputChange('row_path', table.rows(this).data().toArray());
                    });"),
      options = list(
        paging=FALSE,
        searching = FALSE,
        ordering=FALSE,
        scrollY = 750,
        scrollCollapse=TRUE,
        server = TRUE
        )
      )
})
  
  output$infoButton = renderUI({
    s = input$table2_rows_selected # Row number of selected row 
    if (length(s)!= 0) {
      tagList(
        actionButton("info", "",icon("info-circle"),style="color:rgb(57,156,8);border-color:rgb(255,255,255)"),
        
        # Information Dialog Box
        bsModal("ObjectInfo", LeafTable[s,c(1)], "info", size = "large", # Enables Pop up Screen
                
                img(src= paste0("Images/",LeafTable[s,c(1)]),width='800',height='600')
        )
      )
      
    }
  })
  
}

shinyApp(ui = ui, server = server)
