library(shiny)
library(bslib)
library(tidyr)
library(Momocs)


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
                      card(
                        card_header("How to use this website"),
                      "This is a website created to help reasurchers ID large datasets of moth images",
                      "I just want to have lotsss of info on how to create the csv.",
                      "Have picture files, under a column names Moth_URL.",
                      "The first image will pop up on home screen, it should be a full picture of the setup.",
                      "box the measuring tool to the best of your abilities.",
                      "write the true measurments in whatever unit you want cm, in...anything.",
                      "Maby have extra info about possible moth species based on location",),
                      card(card_header("Do's and Don't's of the website"),
                           "DO and donot"),
                      card(card_header("some results "),"YAY, accuracy of the website")
                      
             ),
             tabPanel("Shape Morphology",
                      "Submit the CSV you got from the HOME tab",
                      fileInput("Results",label=""),
                      card(plotOutput("shape")),
                      actionButton("k.means", label="K-Means")
             ),
             tabPanel("Timeline",
                      "Submit the CSV you got from the HOME tab",
                      fileInput("Home2_csv",label=""),
                      card(plotOutput("timeline_plot")),
                      selectInput("filter", "Choose your filter", choices=c("Year","Month","Day","Time")),
                      selectInput("filter2", "Choose your filter", choices=c("Color","Size"))
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
                 box_height=input$box_height)
  })
#=================================================================
  #I want to read the first image off the csv
  firstimage <- reactive({
    req(input$csv_please)
    
    read.csv(input$csv_please$datapath)
  })
  #=================================================================
  output$results <- renderTable({
    processed_data()
  })
  #=================================================================
  output$moth_image <- renderUI({
    req(firstimage())
    img(
      src = firstimage()$Moth_URL[1],
      height = 500,
      width = 500,
      style = "position: absolute;top: 103px;left: 16px;")
    
  })
  #=================================================================
  output$dynamic_floating_box <- renderUI({
    card(
      id = "floating_box",
      style = paste0(
        "left:", input$x_position, "px;",
        "top:", input$y_position, "px;",
        "width:", input$box_width, "px;",
        "height:", input$box_height, "px;",
        "border:2px dotted red;",
        "background:transparent;"))
  })
  #=================================================================  
  
  output$timeline_plot<-renderPlot({
    req(input$Home2_csv)
    
    timeline<- read.csv(input$Home2_csv$datapath)
    #2025:05:16 23:58:15
    metadata_time<-separate(moths,
                            Time_taken....metadata.origin_timestamp,
                            c("date","time"),
                            sep = " ")
    
    metadata_time<-separate(metadata_time,
                            date,
                            c("year","month","day"),
                            sep = ":")
    
    moth<-separate(metadata_time,
                   time,
                   c("hour","minute","second"),
                   sep = ":")
    moth$month<- as.numeric(moth$month)
    hist(moth$month)
  })
  #=================================================================
  output$shape<-renderPlot({
    req(input$Results)   

    moths<- read.csv(input$Results$datapath)
    #cat("csv loaded!")
    #ok so in the csv I mushed all the corrdinates together to save space
    #now we have to un mush them 
    translate<- data.frame()
    
    for(i in 1:nrow(moths)){
      
      seperate <- data.frame(
        coords = moths$Shape....paste.hull_x..hull_y..collapse.......[i]
      )
      
      #                          x1 y1 ; x2 y2
      #This is how it is set up 116 154;116 155;
      #So we want to break it up from space and semicolon 
      seperate<- seperate%>% 
        # %>% is basically a way to say take this and do this other thing with it
        #so like it I wanted to clean up data I could say take this data, subset it, remove the NAs and add it together 
        #dataset%>% subset%>%drop_na%>%sum
        #Apparently there is another version like |> which I saw in the website below and it confused me 
        #It is called a pipe! (yay :D)
        separate_longer_delim(coords, delim = ";")%>% 
        separate_wider_delim(coords, delim = " ", names = c("X", "Y"))
      #https://tidyr.tidyverse.org/reference/separate_longer_delim.html
      
      #I got an error, I guess I need to define my X nd Y as numberic
      
      seperate$X <- as.numeric(seperate$X)
      seperate$Y <- as.numeric(seperate$Y)
      seperate$ID <- i #make sure we can differentiate the moths
      
      translate<- rbind(translate,seperate)
    }
    
    #Ok so now I am gonna try to use MOMOCS AGAIN!!!
    #quick definitions for me to remember
    #- COO: (x,y)<-- coordinat based
    #- OUT: closed outlines
    #- OPN: open outlines
    #- LDK: configurations of landmarks<-- maybe for patterns in the future
    #- COE: coefficients of the shapes
    
    
    #Lovleymoths<-Out(translate, fac = translate$ID)
    # data.frame must have a `coo` column
    #- $coo which is a list of matrices for coordinates
    #-$fac a data_frame for covariates
    
    #SO I need to make the x,y into a matrix?<-- and make sure each one has the ID
    #https://www.statology.org/split-function-in-r/
    #I think I want this but it could be improved!!
    
    #Lovley_moths<-split(translate, translate$ID)
    
    #Lovley_moths[[1]]
    #ok but they are tibbles 
    #so they look like         1{ID,X,Y}
    #as.matrix(Lovley_moths[[1]]) #well that worked now 
    #I want to do this to every group
    #lapply(Lovley_moths, as.matrix) #lapply basically lets me apply it to eveything in that list 
    #I am gonna use my new pipe skills tho :)
    Lovley_moths<-translate%>%
      split(.$ID)%>% #the . basically means you it is being carried in the pipe 
      #so translate--> split(translate,translate&ID)
      #I probally didn't need to do (.,.$ID) or (.,as.matrix) cause the dataset is already established
      lapply(as.matrix)%>% #I okly want X and Y in the matrix not the ID
      lapply(.,function(yay) {yay[, colnames(yay) != "ID"]})
    #the function will let me temporarily call each list and say for the first list 
    #remove the column named ID, and so on 
    
    #my_matrix[, colnames(my_matrix) != 'A']
    #Lovley_moths[[1]] #YAYYYYY
    #Ok now I want to use the OUT object
    #this is the fac value that lets me seperate the outlines :)
    Moth_ID_List<-data.frame(unique(translate$ID)) #I think it needs to be a dataframe
    
    Cute_moth_outlines<-Out(Lovley_moths,fac=Moth_ID_List)
    #list(translate$ID) #I want to remove the repeating numbers
    
    #Lets just see them real quick 
    #panel(Cute_moth_outlines )#some points look rough but honestly it looks pretty nice!
    #Cute_moth_outlines[1] %>% coo_plot() #cool!
    
    #invalid "xlim" value 
    #The error apparently can happen if it is "Non-Numeric Data"
    
    #So I cannot use the proctustes b/c there are no landmarks!
    #stack(Cute_moth_outlines)
    
    #https://momx.github.io/Momocs/reference/efourier.html
    #coo <- Cute_moth_outlines[2]
    #coo_plot(coo)
    #ef <- efourier(coo, 12)
    #efourier(coo, 12, norm=TRUE)
    #efi <- efourier_i(ef)
    #coo_draw(efi, border='red', col=NA)
    maybe<-coo_alignxax(Cute_moth_outlines)
    #stack(maybe)
    m<-coo_slidedirection(maybe,"right")
    
    #stack(m)
    pls_work<- efourier(m,nb.h= 12,norm=TRUE)
    
    #nb.h=The number of harmonics to use
    #smooth.it=The number of smoothing iterations to perform
    #norm= whether or not to normalize the coefficents \
    #- if they look upsidedown, align the outlines b4 hand and turn norm as false
    #start= consider the first point to all be the same 
    
    tunnel_light<-PCA(pls_work)
    tunnel_light
    plot(tunnel_light,pos.shp="xy",labelspoint=TRUE) #OMG IT WORKEDDD
    
    
    
    #ok but BUT I think we have a problem, unlike the bottle image these moths are facing diffrent directions...
    #I should prolly fix this... but HOW 
    #can I say anything on one side of the graph, flip to the other side?
  })
}

shinyApp(ui = ui, server = server)
