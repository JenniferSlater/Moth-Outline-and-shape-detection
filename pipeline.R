library(imager)
library(concaveman)
library(sp)
library(raster)

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
    
    for (i in 1:nrow(data)) {
      mURL <- data$Moth_URL[i]
    
      moth<-load.image(mURL)
    
      #FIX THIS IN FUTURE!!!!!!!!!!!!!!!!!!
      moth<-moth[ , , ,-4] 
      moth<-drop(moth)
      moth <- as.cimg(moth)
    
      #For edge detection 
      #Turn into greyscale
      gmoth<- grayscale(moth)   
      cat("Grayscale worked!\n")
      
      #Create gassian blur
      #calling for the x and y from this x,y,c
      image_width<-dim(gmoth)[1] #total length along the x
      image_height<-dim(gmoth)[2] #total length along the y
  
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
      red_orange_mask<- ((H>10)&(H<=20))&!(black_mask)&exclution
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
      plot(black,main="black")
      
      white<- (white_mask) & (binary_field[]==1)
      plot(white,main="white")
      
      light_grey<-(light_grey_mask) & (binary_field[]==1)
      plot(light_grey, main="light_grey")
      
      dark_grey<-(dark_grey_mask) & (binary_field[]==1)
      plot(dark_grey, main="dark_grey")
      
      grey<- (grey_mask) & (binary_field[]==1)
      plot(grey, main="grey")
      
      
      red<-(red_mask) & (binary_field[]==1)
      plot(red,main="red")
      
      red_orange<-(red_orange_mask) & (binary_field[]==1)
      plot(red_orange,main="red_orange")
      
      orange_brown<- (orange_brown_mask) & (binary_field[]==1)
      plot(orange_brown,main="orange_brown")
      
      orange_yellow<- (orange_yellow_mask) & (binary_field[]==1)
      plot(orange_yellow, main="orange_yellow")
      
      yellow<- (yellow_mask) & (binary_field[]==1)
      plot(yellow, main="yellow")
      
      yellow_green<- (yellow_green_mask) & (binary_field[]==1)
      plot(yellow_green, main="yellow_green")
      
      green<- (green_mask) & (binary_field[]==1)
      plot(green, main="green")
      
      green_cyan<- (green_cyan_mask) & (binary_field[]==1)
      plot(green_cyan, main="green_cyan")
      
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
      Top_color<-Color_percentages$Color_names[which.max(Color_percentages$Amount)]
      cat("The top color is",Top_color,"\n")
      
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
      
      # I am pretty happy with this so I am ok with 
      width_pixel_conversion<-true_width/box_width
      height_pixel_conversion<-true_height/box_height
      cat(width_pixel_conversion, "px/measure\n")
      cat(height_pixel_conversion, "px/measure\n")
      
      area_of_moth<-((width_pixel_conversion+height_pixel_conversion)/2)*pixel_count
      cat("the surface area of the moth is", area_of_moth,"!\n")
      moth_width_cm <- max((hull_x)*width_pixel_conversion) - min((hull_x)*width_pixel_conversion)
      cat("the moth width is", moth_width_cm,"!\n")
      moth_height_cm <- max((hull_y)*height_pixel_conversion) - min((hull_y)*height_pixel_conversion)
      cat("the moth height is", moth_height_cm,"!\n")
      
      
    
  }}}
