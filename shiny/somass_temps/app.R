library(shiny)
library(ggplot2)

ui <- fluidPage(
  tabPanel(
    "Upload File",
    titlePanel("Uploading Files"),
    sidebarLayout(
      sidebarPanel(
        
        fileInput('file1', 'Choose CSV File',
                  accept=c('.csv')),
        
        checkboxInput('header', 'Header', TRUE),
        
        radioButtons('sep', 'Separator',
                     choices = c(Comma=',',
                               Semicolon=';', Tab='\t'),
                     selected = ','),
        
        selectInput('xcol', 'X Variable', 
                    "", selected = NULL),
        
        selectInput('ycol', 'Y Variable', 
                    "", selected = NULL)
        
      ),
      mainPanel(
        tableOutput('contents'),
        plotOutput('MyPlot')
      )
    )
  )
)

server <- function(input, output, session) {
  
  myfiles <- reactive({
    req(input$file1$datapath, 
        file.exists(input$file1$datapath))
    read.csv(input$file1$datapath)
  })
  
  observeEvent(myfiles(), {
    req(myfiles())
    nms <- colnames(myfiles())
    updateSelectInput(
      session, 
      inputId = 'xcol', 
      label = 'X Variable',
      choices = nms, selected = nms[1]
    )
    
    updateSelectInput(
      session, 
      inputId = 'ycol', 
      label = 'Y Variable',
      choices = nms, selected = nms[1]
    )
  })
  
  
  df <- reactive({
    dplyr::tibble(myfiles()) 
    
  })

  output$MyPlot <- renderPlot({
    req(myfiles(), input$xcol, input$ycol)
    ggplot(data = df(), 
    mapping = aes_string(input$xcol, input$ycol)) +
      geom_point() +
      theme_gray() +
      geom_smooth(method = "lm", color = "black",
                linetype = 2, alpha = 1/5)
  })
}

shinyApp(ui , server)