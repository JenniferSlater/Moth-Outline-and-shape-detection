library(imager)
library(concaveman)
library(sp)
library(raster)
library(exif)

#set up csv
CSV_results<-data.frame(
  Moth_URl=character(),
  Surface_area=numeric(),
  Width=numeric(),
  Height=numeric(),
  Color_1=character(),
  Color_1_P=numeric(), #stands for percentage 
  Color_2=character(),
  Color_2_P=numeric(),
  Color_3=character(),
  Color_3_P=numeric(),
  Shape_X=numeric(),
  Shape_Y=numeric(),
  File_date=character(),
  Time_taken=character(),
  Location_X=character(),
  Location_Y=character()
)

#ok so this links to the app.R shiny app
run_pipeline <- function(csv,
                         true_width,
                         true_height,
                         box_width,
                         box_height) {
  
  cat("True width:", true_width, "user's measurment\n")
  cat("True height:", true_height, "user's measurment\n")
  cat("Box width:", box_width, "px\n")
  cat("Box height:", box_height, "px\n")
  
  
  data <- read.csv(csv)
  
  if (!"Moth_URL" %in% colnames(data)){ #If we can't see anything, just mark as true
    message("!!please label the first row Moth_URL!!")
  }
  
  else {
    #The conversion should be based on the first image in the csv since that is what the users use 
    mURL <- file.path("www",data$Moth_URL[1])
    moth<-load.image(mURL)
    
    image_width<-dim(moth)[1]
    image_height<-dim(moth)[2]
    
    #The users pixel measurements are based on a 500*500 image
    #while the real image could be something crazy like 4000px* 3000px
    xscale<-image_width/500  
    yscale<-image_height/500
    #now we convert those box pixels widths with this!
    box_width_withscale<-box_width*xscale
    box_height_withscale<-box_height*yscale
    
    width_pixel_conversion<-true_width/box_width_withscale
    height_pixel_conversion<-true_height/box_height_withscale
    cat(width_pixel_conversion, "unit|pixel\n")
    cat(height_pixel_conversion, "unit|pixel\n")
    
    
    for (i in 2:nrow(data)) {
      mURL <- file.path("www",data$Moth_URL[i])
      
      moth<-load.image(mURL)
      
      #FIX THIS IN FUTURE!!!!!!!!!!!!!!!!!!
      #moth<-moth[ , , ,-4] 
      #moth<-drop(moth)
      #moth <- as.cimg(moth)
      
      #For edge detection 
      #Turn into greyscale
      gmoth<- grayscale(moth)   
      cat("Grayscale worked!\n")
      
      #Create gassian blur
      #calling for the x and y from this x,y,c
      image_width<-dim(gmoth)[1] #total length along the x
      cat("image width", image_width,"\n")
      image_height<-dim(gmoth)[2] #total length along the y
      cat("image height", image_height,"\n")
      blur<-sqrt(image_width*image_height)*0.03
      #This blur is pretty arbitrary, I wanted the length and width to affect the blur 
      #smaller images would need less of a blur, and I also needed the calculated blur to be quite small
      #thus I just square rooted it and multiplied by a decimal changing it until it worked :)
      
      blur_moth<- isoblur(gmoth,blur,neumann = TRUE, gaussian = TRUE)
      #Neumann is also true to help normalize the edges of the box
      
      plot(blur_moth, main="blurred moth :)")
      cat("Gaussian blur worked!\n")
      #If it says can't load CHECK YOUR CONNECTION to the internet
      
      #CANNY EDGE DETECTION
      canny_moth<-cannyEdges(blur_moth,sigma=blur)
      #I am using that same blur varibale for this since it changes with size
      lines<-which(canny_moth, arr.ind = TRUE) 
      if (nrow(lines)==0){ #If we can't see anything, just mark as true
        message("moth in",mURL,"undetected")
        next}
      cat("Canny edge detection worked!\n") #yay!!
      
      hull<-concaveman(lines, concavity = 4, length_threshold = 0)
      #hull is a matrix with 4 colums [1] 483   4
      # ex(     V1  V2 V3 V4)
      #[1,] 99 182  1  1
      hull_x<-hull[,1]
      hull_y<-hull[,2]
      
      plot(x=hull_x,y=hull_y, col="red", main="moth outline")
      lines(hull,col="blue")
      cat("Concaveman worked!\n")
      
      #https://mhallwor.github.io/_pages/basics_SpatialPolygons
      poly<-Polygon(cbind(hull_x, hull_y))
      ps<-Polygons(list(poly),1)
      sps<-SpatialPolygons(list(ps))
      plot(sps,xlim=c(0,image_width), ylim=c(0,image_height), main="polygon") #ok perfect it is plotted :)
      cat("Polygon worked!\n")
      
      #plotting raster image 
      
      # create blank raster layer                 #needs to match image dimentions 
      blank_canvas<-raster(nrows=image_height, ncols=image_width, xmn=0, xmx=image_width, ymn=0, ymx=image_height)
      
      #rasterImage(image, xleft, ybottom, xright, ytop, angle = 0, interpolate = TRUE, ...)
      #    minx   max y               max x               min y
      raster_map<-rasterImage(moth, xleft=0,ybottom=image_height,xright=image_width, ytop=0,interpolate = FALSE)
      plot(sps,add=TRUE, main="polygon matched up with image")
      
      binary_field<-rasterize(x=sps,y=blank_canvas,field=1,background=0) 
      plot (binary_field, "Binary field, white=1, black = 0")
      flipped_moth<-mirror(moth,"y")#this can be seen in the rasterImage, it needs to be flipped
      
      # colors of pixels 
      #Step 11: CONVERT HSV
      #I am doing this because HSV is easier to quantify into ranges 
      color_hsv <- RGBtoHSV(flipped_moth)
      H <- channel(color_hsv,1) #Hue
      S <- channel(color_hsv,2) #Saturation 
      V <- channel(color_hsv,3) #Value
      #-----------------------------------------------
      #Step 12: COLOR RANGES
      #https://data.europa.eu/apps/data-visualisation-guide/describing-colours-hsl
      #https://pseudopencv.site/utilities/hsvcolormask/
      #THESE ARE THE COLOR RANGES FOR THE HSV 
      
      #someday it would be really cool to use open CV (cv2), with the upper and lower limits
      #0,0,5
      black_mask<-      (V<=15/100)&               (S<=20/100)
      dark_grey_mask<- ((V>15/100)&  (V<40/100)) &((S<=20/100))
      grey_mask<-      ((V>=40/100)& (V<75/100)) & (S<=20/100)
      light_grey_mask<-((V>=75/100)& (V<80/100)) & (S<=20/100)
      white_mask <-     (V>=80/100)&               (S<=20/100)
      
      #Adding other pretty colors :)
      #https://www.workwithcolor.com/yellow-green-color-hue-range-01.htm
      #If we look at a color like red it is both at 360 and 0, so I put an OR statement(that's y the line)
      
      #I got tires of trying to figure out all the saturations and values for this
      exclution<- !(black_mask)&!(dark_grey_mask)&!(grey_mask)&!(light_grey_mask)&!(white_mask)
      
      red_mask<- (((H>=0)&(H<=10))|((H>355)&(H<=360)))&exclution
      red_orange_mask<- ((H>10)&(H<=20))&exclution
      orange_brown_mask<-((H>=20)&(H<41))&exclution   #It would be nice to in the future to add tans and browns (that would need to mess with V tho)
      orange_yellow_mask<-((H>=41)&(H<=50))&exclution
      yellow_mask<- ((H>50)&(H<=60))&exclution
      yellow_green_mask<- ((H>60)&(H<=80))&exclution
      green_mask<- ((H>80)&(H<=140))&exclution
      green_cyan_mask<- ((H>140)&(H<=169))&exclution
      cyan_mask<- ((H>169)&(H<=200))&exclution
      cyan_blue_mask<- ((H>200)&(H<=220))&exclution
      #I would be shocked it a moth was one of these colors, but gotta follow through (maybe could help indicate issue with lighting)
      blue_mask<- ((H>220)&(H<=240))&exclution
      purple_mask<- ((H>240)&(H<=280))&exclution
      magenta_mask<- ((H>280)&(H<=320))&exclution
      magenta_pink_mask<- ((H>320)&(H<=330))&exclution
      pink_mask<- ((H>330)&(H<=345))&exclution
      pink_red_mask<- ((H>345)&(H<=355))&exclution
      #-----------------------------------------------
      # black, white, dark_grey, grey, brown , red, red_orange, orange, orange_yellow, yellow, yellow_green, green, green_cyan, cyan, cyan_blue, blue, purple, magenta, magenta_pink ,pink, pink_red     
      #REMEMBER TO COMMENT ALL PLOTS OUT EVENTUALLY!!!
      black<- (black_mask) & (binary_field[]==1) #want the field to be true :)
      #plot(black,main="black")
      
      white<- (white_mask) & (binary_field[]==1)
      #plot(white,main="white")
      
      light_grey<-(light_grey_mask) & (binary_field[]==1)
      #plot(light_grey, main="light_grey")
      
      dark_grey<-(dark_grey_mask) & (binary_field[]==1)
      #plot(dark_grey, main="dark_grey")
      
      grey<- (grey_mask) & (binary_field[]==1)
      #plot(grey, main="grey")
      
      red<-(red_mask) & (binary_field[]==1)
      #plot(red,main="red")
      
      red_orange<-(red_orange_mask) & (binary_field[]==1)
      #plot(red_orange,main="red_orange")
      
      orange_brown<- (orange_brown_mask) & (binary_field[]==1)
      #plot(orange_brown,main="orange_brown")
      
      orange_yellow<- (orange_yellow_mask) & (binary_field[]==1)
      #plot(orange_yellow, main="orange_yellow")
      
      yellow<- (yellow_mask) & (binary_field[]==1)
      #plot(yellow, main="yellow")
      
      yellow_green<- (yellow_green_mask) & (binary_field[]==1)
      #plot(yellow_green, main="yellow_green")
      
      green<- (green_mask) & (binary_field[]==1)
      #plot(green, main="green")
      
      green_cyan<- (green_cyan_mask) & (binary_field[]==1)
      #plot(green_cyan, main="green_cyan")
      
      cyan<-(cyan_mask) & (binary_field[]==1)
      #plot(cyan, main="cyan")
      
      cyan_blue<- (cyan_blue_mask) & (binary_field[]==1)
      #plot(cyan_blue, main="cyan_blue")
      
      blue<- (blue_mask) & (binary_field[]==1)
      #plot(blue, main="blue")
      
      purple<- (purple_mask) & (binary_field[]==1)
      #plot(purple, main="purple")
      
      magenta<-(magenta_mask) & (binary_field[]==1)
      #plot(magenta, main="magenta")
      
      magenta_pink <-(magenta_pink_mask) & (binary_field[]==1)
      #plot(magenta_pink, main="magenta_pink")
      
      pink<-(pink_mask) & (binary_field[]==1)
      #plot(pink, main="pink")
      
      pink_red<-(pink_red_mask) & (binary_field[]==1)
      #plot(pink_red, main="pink_red")
      
      cat("Colors were found!\n")
      #YAY I am very happy with this so far :)
      #-----------------------------------------------
      #Number of pixels
      binary<-(binary_field==1)
      pixel_count<-cellStats(binary,"sum") #wow that was easy ;-;
      cat(pixel_count,"is total pixels\n")
      
      #Quantify colors 
      Color_percentages<- data.frame(
        Color_names<-c("black", "white", "dark_grey","light_grey" ,"grey" , "red", "red-orange", "orange_brown", "orange-yellow", "yellow", "yellow-green", "green", "green-cyan", "cyan", "cyan-blue", "blue", "purple"," magenta","magenta_pink" ,"pink", "pink-red"),
        Amount<-c(sum(black), 
                  sum(white), 
                  sum(dark_grey), 
                  sum(light_grey),
                  sum(grey),
                  sum(red), 
                  sum(red_orange), 
                  sum(orange_brown), 
                  sum(orange_yellow), 
                  sum(yellow), 
                  sum(yellow_green), 
                  sum(green), 
                  sum(green_cyan), 
                  sum(cyan), 
                  sum(cyan_blue), 
                  sum(blue), 
                  sum(purple), 
                  sum(magenta), 
                  sum(magenta_pink) ,
                  sum(pink), 
                  sum(pink_red))
      )
      
      
      total_colorPixels<-sum(Color_percentages$Amount)
      cat("There were", total_colorPixels, "pixels detechted using my color range\n")
      cat("That means that",pixel_count-total_colorPixels,"pixels didn't fall in the ranges\n")
      #I want the answer to be 0
      #If it is a negative there must be an overlap with the color ranges
      #If it is positive the color ranges do not catch enough
      Color_percentages<-Color_percentages[order(Color_percentages$Amount, decreasing=TRUE),]
      Top_three<- head(Color_percentages, 3)
      
      Top_C<-Top_three$Color_names[1]
      Top_A<-((Top_three$Amount[1])/total_colorPixels)*100
      
      
      Second_C<-Top_three$Color_names[2]
      Second_A<-((Top_three$Amount[2])/total_colorPixels)*100
      
      Third_C<-Top_three$Color_names[3]
      Third_A<-((Top_three$Amount[3])/total_colorPixels)*100
      
      #error  check!!!
      xx<-summary(H)
      #cat("summary H",xx,"\n")
      xx<-summary(S)
      #cat("summary S",xx,"\n")
      xx<-summary(V)
      #cat("summary V",xx,"\n")
      ay<-range(H)
      yy<-range(S)
      cc<-range(V)
      #cat("H",ay,"S",yy,"v",cc,"\n")
      
      #This is using the conversion based in the first image 
      area_of_moth<-(width_pixel_conversion*height_pixel_conversion*pixel_count)
      cat("the surface area of the moth is", area_of_moth,"units^2!\n")
      moth_width_cm <- ((max(hull_x) - min(hull_x))*width_pixel_conversion)
      cat("the moth width is", moth_width_cm,"units!\n")
      moth_height_cm <- ((max(hull_y) - min(hull_y))*height_pixel_conversion)
      cat("the moth height is", moth_height_cm,"units!\n")
      
      metadata1<-read_exif(mURL)
      
      CSV_results <- rbind(
        CSV_results,
        data.frame(
          
          Moth_URL<- mURL,
          Surface_area<-area_of_moth,
          Width<-moth_width_cm,
          Height<-moth_height_cm,
          Color_1<-Top_C,
          Color_1_P<- Top_A,
          Color_2<-Second_C,
          Color_2_P<-Second_A,
          Color_3<-Third_C,
          Color_3_P<-Third_A,
          Shape_X<-paste(hull_x,collapse=";"),
          Shape_Y<-paste(hull_y,collapse=";"),
          File_date<-metadata$timestamp,
          Time_taken<- metadata$origin_timestamp,
          Location_X<- metadata1$longitude,
          Location_Y<-metadata1$latitude
        ))
      
    }}
  
  write.csv(CSV_results, "Morphometric_moth_results.csv")
  
  
  }


