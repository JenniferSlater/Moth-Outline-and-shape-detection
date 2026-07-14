library(shiny)
library(bslib)

source("pipeline.R")
# User interface 
ui<- page_fluid(
  
  page_sidebar(
    title= "title panel",
    
      
    sidebar = sidebar(position = "left",
                      
                      #Sliders 
                      "Box your measurment tool in!",
                      sliderInput("x_position", "Change the X position:", min = 0, max = 500, value = 50),
                      sliderInput("y_position", "Change the Y position:", min = 0, max = 500, value = 50),
                      sliderInput("box_width", "Box width:", min = 0, max = 500, value = 50),
                      sliderInput("box_height", "Box height:", min = 0, max = 500, value = 50),
                      "",
                      numericInput("true_width","What the width of your measurment tool?", value=NA),
                      numericInput("true_height","What the heigth of your measurment tool?",value =NA),
                      actionButton("submit","Submit the measurments")),
  

  
  mainPanel(
    tableOutput("results")
    ),
      
      card(
        card_header("WOAH"),
        "hello Hello Hello!",
        
           div(
      style = "
    position: relative;
    width: 500px;
    height: 500px;
  ",
      uiOutput("moth_image"),
      
      uiOutput("dynamic_floating_box")),
        
        fileInput("name",label="upload"),
        
        card_footer("more stuff"))
    
    
    
#*Output function in the ui to place reactive objects in your Shiny app,
  ))

# Server Function

server <- function(input, output) {
  
  processed_data <- eventReactive(input$submit,{
    req(input$name)
    req(input$true_width)
    req(input$true_height)
    req(input$box_width)
    req(input$box_height)
    run_pipeline(csv=input$name$datapath,
                 true_width=input$true_width,
                 true_height=input$true_height,
                 box_width=input$box_width,
                 box_height=input$box_height
                 )
  })
  
#I want to read the first image off the csv
  firstimage <- reactive({
    req(input$name)
      
    read.csv(input$name$datapath)
  })
  
  output$results <- renderTable({
    processed_data()
  })
  
  output$moth_image <- renderUI({
    req(firstimage())
    img(
      src = firstimage()$Moth_URL[1],
      height = 500,
      width = 500,
      style = "
        position: absolute;
        top: 0px;
        left: 0px;")
  
  })
  
  output$dynamic_floating_box <- renderUI({
    card(
      id = "floating_box",
      style = paste0(
        "position:absolute;",
        "left:", input$x_position, "px;",
        "top:", input$y_position, "px;",
        "width:", input$box_width, "px;",
        "height:", input$box_height, "px;",
        "border:2px solid red;",
        "background:transparent;",
        "pointer-events:none;"
      )
    )
    })
  
}

shinyApp(ui = ui, server = server)

