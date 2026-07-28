library(shiny)
library(bslib)

source("pipeline.R")
# User interface 
ui<- page_fluid(
  tags$head(
    tags$style(HTML("
          body {
        background-color: #e7f1dc;}
                    "))),
  
  navbarPage(title="Moth Morphometrics!", #ok this is cute :D
             tabPanel("Home",
                      sidebarLayout(
                        sidebarPanel(
                          #Sliders 
                          fileInput("csv_please",label="Upload your CSV file"),
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
                          tableOutput("results"),
                          card(
                            card_header("WOAH"),
                            "hello Hello Hello!",
                            
                            div(
                              style = "width: 500px; height: 500px;",
                              uiOutput("moth_image"),
                              uiOutput("dynamic_floating_box")),
                            card_footer("more stuff"))
                          
                        ))),
             tabPanel("Guide",
                      "There is stuff in here.\n",
                      "I just want to have lotsss of info on how to create the csv.",
                      "Have picture files, under a column names Moth_URL.",
                      "The first image will pop up on home screen, it should be a full picture of the setup.",
                      "box the measuring tool to the best of your abilities.",
                      "write the true measurments in whatever unit you want cm, in...anything.",
                      "Maby have extra info about possible moth species based on location"
             ),
             tabPanel("K-means clustering",
                      "Submit the CSV you got from the HOME tab",
                      fileInput("Home_csv",label="Upload your CSV file"),
                      actionButton("color","Color"),
                      
             ),
             tabPanel("Timeline",
                      "Submit the CSV you got from the HOME tab",
                      fileInput("Home_csv",label="Upload your CSV file"),
             ),
             
  ))


# Server Function

server <- function(input, output) {
  
  processed_data <- eventReactive(input$submit,{
    req(input$csv_please)
    req(input$true_width)
    req(input$true_height)
    req(input$box_width)
    req(input$box_height)
    run_pipeline(csv=input$csv_please$datapath,
                 true_width=input$true_width,
                 true_height=input$true_height,
                 box_width=input$box_width,
                 box_height=input$box_height
    )
  })
  
  #I want to read the first image off the csv
  firstimage <- reactive({
    req(input$csv_please)
    
    read.csv(input$csv_please$datapath)
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
      style = "position: absolute;top: 103px;left: 16px;")
    
  })
  
  output$dynamic_floating_box <- renderUI({
    card(
      id = "floating_box",
      style = paste0(
        "left:", input$x_position, "px;",
        "top:", input$y_position, "px;",
        "width:", input$box_width, "px;",
        "height:", input$box_height, "px;",
        "border:2px dotted red;",
        "background:transparent;"
      )
    )
  })
  
}

shinyApp(ui = ui, server = server)
