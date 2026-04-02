#Stuff to install :)
#install.packages("imager")
#install.packages("FNN")
#install.packages("sf")
#install.packages("smoothr")
#install.packages("raster")
#install.packages("exifr")
#install.packages("momocs")

#**SEE METADATA SECTION (YOU HAVE TO DOWNLOAD SOMETHING FOR THIS!!)
#install.packages("exifr")

library(imager)
library(ggplot2)
#Load images from github :)
 #---------------CREATE RESULTS CSV TO SAVE TO--------------------------------------------------- 
#just setting up what I want the csv to have outside the for loop
results_CSV<-data.frame(
  Moth_URL=character(),
  Color_URL=character(),
  Width=numeric(),
  Height=numeric(),
  Color=character(),
  Shape=character(),
  File_Date=character(),
  Time_Taken=character(),
  Location_X=character(),
  Location_Y=character()
)
 #---------------------LOAD IN UNKNOWN MOTH CSV---------------------------------------------------
#ok so I made a csv to make this process easier
#I will have the program go through the csv and save the coordinates, as well as the size and color
#Then I will do the same with known shapes and I will use those to help with momocs
#If I get that far...
CSV_unknownMoths<-read.csv("C:/Users/slate/Desktop/UnknownMothTest.csv", header=TRUE)

for (i in 1:nrow(CSV_unknownMoths)) {
  mURL <- CSV_unknownMoths$Moth_URL[i]
  cURL <- CSV_unknownMoths$Color_URL[i]

  moth<-load.image(mURL)           #If it says can't load CHECK YOUR CONNECTION to the internet
  colorscale<-load.image(cURL)
  
  #----------------------FIX PNGS ----------------------------------------------------  
#IF YOU HAVE JPEGS COMMENT THIS OUT, THIS IS FOR PNGS!!!!!!!!!!!
#I forgot that I have an issue with pngs...
#we r gonna try to error fix cause that was a lot of work I don't want to redo...
#last time I had an issue cause png's have 4 color channels
#(x,y,c<--colorchannel,z<--frames)
  #inside c we have RGB and alpha
  
# alphs is just extra info so I am going to TRY to eliminate it
#IF YOU HAVE AN ERROR AT THIS STAGE LOOK AT THE DIM, YOU WANT 3!!!!
#https://cran.r-project.org/web/packages/recolorize/vignettes/step01_loading.html
    #KEEP ONLY X,Y,C
    #684 584   1   4                                        123 4
  moth<-moth[ , , ,-4] #include everything except 4 (in the RBG ALPHA)
  moth<-drop(moth)#removes 1's and cause the png Z is 1 it should work
  #The error continues....
  
  moth <- as.cimg(moth)#I think it must not be recognized as an image (sees it as an array) anymore so I am going to remake it as an image
  
  colorscale <- colorscale[ , , ,-4]
  colorscale<-drop(colorscale)
  colorscale <- as.cimg(colorscale)
#AHHHH IT WORKED :)))))
  
#--------------------INDIVIDUAL IMAGES-----------------------------------
#if you want to do a individual image, this is how you do it...
#moth <- load.image("C:/Users/slate/Desktop/colorbar (2).JPG")
#plot(moth,main="Color Scale") 

#colorscale <- load.image("C:/Users/slate/Desktop/colorbar (2).JPG")
#plot(colorscale,main="Color Scale") 
  
#---------------CANNY EDGE DETECTION--------------------------------------------------------

#Step 1: TURN TO GREYSCALE
#https://justin-liang.com/tutorials/canny/ 
  gmoth <- grayscale(moth)
#plot(gmoth, main="Greyscale of Moth")
#-----------------------------------------------
  
#Step 2: GAUSSIAN BLUR 
#so it very varied for me if the moth image was small
#bmoth<-isoblur(gmoth,5,neumann = TRUE, gaussian = TRUE, na.rm = TRUE)

#calling for the x and y from this x,y,c<--colorchannel
  Imagex<-dim(gmoth)[1] #x 
  Imagey<-dim(gmoth)[2] #y
#print(Imagex)
#print (ImageY)<--quick cutesy little test

#this basically will look @the height of pixels 
#Imagex*Imagey<--total pixels
#I don't really want the total area, I square rooted it so I had less to work with
  blurcalc<-sqrt(Imagex*Imagey)*0.03 #<-- the higher the number the more blur
  print(blurcalc)

  bmoth<-isoblur(gmoth,blurcalc,neumann = TRUE, gaussian = TRUE)
  plot(bmoth, main="Moth with Gaussian Blur")
#so IDEALLY this should change with the pixel number
  #-----------------------------------------------

#step 3: CANNY EDGE DETECTION
#https://blog.roboflow.com/edge-detection/
#Gradient magnitude did not work that well, it is not worth further exploring 
  Uncanny1 <- cannyEdges(bmoth, sigma=blurcalc)
#a smaller <(0.5-1) sigma willperserve the find details
#a larger sigma >2 is agressive and blurs edges (it may merge close edges)
#I honestly want a really agressive edge detection
#I tried the blurcalc with it and it works pretty good
  plot(Uncanny1, main="CannyEdges normal")
  #-----------------------------------------------                             Potential improvment (expanding and shrinking pixels)

#Step 5: TURN INTO DATASET
# I found out for fnn to work I need to make it into a numeric matrix
#cause right now it is just a bunch of true and false values
  coords <- which(Uncanny1, arr.ind = TRUE) #looks for true values
  
  wing_points2 <- data.frame(
    yy2=coords[,1], #this will be my y corrd/row
    xx2=coords[,2] #this will be my x coord/colum 
  #they are flipped<--tested it with a graph
  )
#str(coords)

  #---------------FASTEST NEAREST NEIGHBOR--------------------------------------------------------

#step 6: CONNECT WITH FNN
  library(FNN) #Fast Nearest Neighbor Search

#I want to essentially take a point and remove it from the dataset
  unvisited_point<-coords#<--I want to start off with all the info in the matrix being marked as unvisited
  path<-unvisited_point[1,,drop=FALSE]#<--starting with the first point off the list!

  while(nrow(unvisited_point)>0){ #<--while there are still points in unvisited
    mostrecentpt<-(tail(path,1))#<stores the last point, and 1 just makes it a single object:)
  
    Fnnlib<- get.knnx(data=unvisited_point,query=mostrecentpt,k=1, algorithm = "kd_tree")
  #k should be less than sample size!
  #https://www.rdocumentation.org/packages/FNN/versions/1.1.4.1
  #I may try out diffrent algorithms...DO NOT TRY COVER TREE IT CRASHED SO BADDD!!!
  # get.knnx(data, query, k=10, algorithm=c("kd_tree", "cover_tree", "CR", "brute"))
  
    nearestpoint <- Fnnlib$nn.index[1]
    test<-Fnnlib$nn.dist[1]
  
  #lets put it all together :)
    path<-rbind(path,unvisited_point[nearestpoint,,drop=FALSE])
  #bind_rows does not work for matrices apparently
    unvisited_point<-unvisited_point[-nearestpoint,,drop=FALSE]
  #head(unvisited)#<just to make sure its good
  }
#visualize it
  plot(path)

  #---------------MAKE INTO POLYGON--------------------------------------------------------
#Step 7: CLOSE PATH
  closedatpath<- rbind(path, path[1,])                                                       #Potential improvement (remove the jump this creates)      
  #contains 85 444 1 1
  #-----------------------------------------------
  
#Step 8: CONVERT TO SF POLYGON
  #so smooth r works on sf geometrics not matrices
  library(sf) #I think I want to convert to polygon...

##https://mgimond.github.io/Spatial/anatomy-of-simple-feature-objects.html
##https://www.youtube.com/watch?v=BgsN-tpolZM&t=272s
  geome<- st_sfc(st_polygon(list(as.matrix(closedatpath))))         
  ge_sf <- st_sf(geometry = geome)
#I got an error, it says use st_zm() to drop m
  ge<-st_zm(ge_sf)#<-- removes the m dimension (st_sf will add it and we don't want it)
  
  #---------------USE SMOOTHR--------------------------------------------------------
#Step 9: APPLY SMOOTHR CLEANING
#essentially this should help with removing islands and simplifying the overall shape
#https://github.com/mstrimas/smoothr
  library(smoothr)
  polgeo1 <- st_simplify(ge, dTolerance = 4)
  plot(polgeo1, border = "black", main = "simplify")

  crumbs <- drop_crumbs(polgeo1, threshold = 100)

#the it will not fill anything without the plots being simplified first
  mothfill <- fill_holes(crumbs, threshold = 100)
  plot(mothfill, col = "black", main = "Filled Gaps")

  chosen_one <- smooth(mothfill, method = "chaikin", refinements = 3)
  plot(st_geometry(chosen_one), col = NA, border = "blue",lwd = 2,lty = 2,add = TRUE)
  
  #############
  cordinates_Polygon <- st_coordinates(chosen_one)[,1:2]
  colnames(cordinates_Polygon) <- c("x","y")# <--this is really the only way ik how to do this
#I should change in the future to update for moth name and family and...ect
  plot(cordinates_Polygon)
  
  #---------------COLOR RANGES--------------------------------------------------------
#we bring the colors bars back
#all I am going to do is use my old code but now the color bars are isolated >:)
#perfect for my evil scheme
  
#Step 10: BLUR
  #redo the blur like I did for moth but less cause it worked so goood <3
  Colorx<-dim(colorscale)[1] #x
  Colory<-dim(colorscale)[2] #y

  blurcolor<-sqrt(Colorx*Colory)*0.03 #<-- the higher the number the more blur
  bcolorscale<-isoblur(colorscale,blurcolor,neumann = TRUE, gaussian = TRUE)
  
  plot(bcolorscale,main="The blurred colorscale")
  #-----------------------------------------------
  
#Step 11: CONVERT HSV
  color_hsv <- RGBtoHSV(bcolorscale)
  H <- channel(color_hsv,1)
  S <- channel(color_hsv,2)
  V <- channel(color_hsv,3)
  #-----------------------------------------------

#Step 12: COLOR RANGES
#THESE ARE THE COLOR RANGES FOR THE HSV (we will need to tweak it eventually to fit with color correction)
  #d
  white_mask <- ((H > 0) & (H < 160) &(S < 0.2)& (V > 0.8))
  black_mask <- (V < 0.2)
#dim(black_mask)

#Adding other pretty colors :)
  green_mask <- ((H > 90) & (H < 160) & (S > 0.2) &(S < 0.8)& (V > 0.2)& (V < 0.8))
  blue_mask <- ((H > 220) & (H < 250) & (S > 0.2) &(S < 0.8)& (V > 0.2)& (V < 0.8))
  lightblue_mask <- ((H > 165) & (H < 210) & (S > 0.2) &(S < 0.8)& (V > 0.2)& (V < 0.8))

# took me an hour to learn this but Hue is a circular range 
#SOOO...if we look at a color like red it is both at 360 and 0, so I put an OR statement(that's y the line)
#basically from 0-30 OR 306-330
  red_mask <- (((H >= 0 & H <= 30) | (H >= 330 & H <= 360))& (S > 0.2) &(S < 0.8)& (V > 0.2)& (V < 0.8))
  yellow_mask <- ((H > 55) & (H < 75) & (S > 0.2) &(S < 0.8)& (V > 0.2)& (V < 0.8))
  grey_mask <- ((S < 0.2) & (V > 0.2) & (V < 0.8))
  #-----------------------------------------------

#Step 13: LIST ALL COLORS
#MASKING PORTION DONE!!! (now the hard part, figuring out size)
  mask <- list(black=black_mask,   #I tried them separately and it looked good so I am going to loop this
               green=green_mask,
               blue=blue_mask,
               red=red_mask,
               yellow=yellow_mask
  )
  
#---------------BOUNDING BOXES--------------------------------------------------------
  rectangle_sizes <- list() #empty list, I am gonna have it add as it finds the rectangles
  #I have it so it is set up with width,height,color(for identification
  
#Step 14: FIND THE COLOR PIXELS
  for (colors in names(mask)){ 
  
   mask_rect <- mask[[colors]] #calling the colors
   #is this pixel in the color red? (binary t/f)
  
   mask_blur <- isoblur(mask_rect, 10) #I ended up having to clean them up I got no choice
   #bluring then will blur 0 and 1 values into (0.1,0.5,0.6)
   mask_thresh <- mask_blur > 0.5  #basically will eliminate pixels with lower levels
   #by applying the thresh it turns it back to binary(t/f)
  
  labeled_mask <- label(mask_thresh)#label what color it is
   num_regions <- max(labeled_mask) #each section
  
   #print(mask)
   #print(num_regions) 
   plot(mask_thresh)
   #-----------------------------------------------
   
#Step 15: MAKE THE BOUNDING BOXES
   for (region in 1:num_regions) {  #make a loop to go through all the regions
     pix <- which(labeled_mask == region, arr.ind = TRUE)
      #color name              #array thingy for min and max
     xmin <- min(pix[,2]) 
     xmax <- max(pix[,2])
     ymin <- min(pix[,1])
     ymax <- max(pix[,1])
    
      rect( #had to flip them cause it went the wrong way
        xleft = ymin,
        ybottom = xmax,
        xright = ymax,
        ytop = xmin,
        border = "blue",
        lwd =1 #line width so we can see it
      )
    #-----------------------------------------------

#Step 16: DETERMINE LENGTH AND WIDTH OF ALL BOXES                            #Potential improvement (create a method to remove outliers b4 they are calculated in)
    #print(sprintf("Region %d: xmin=%d xmax=%d ymin=%d ymax=%d", 
    #              region, xmin, xmax, ymin, ymax)) #tells me position
     width <- xmax - xmin 
     height <- ymax - ymin
     print(sprintf("Region %d: width = %f px, height = %f px", 
                   region, width, height))#tells me height and width
    
    #Now I wanna just list all the rectangle sizes real quick 
    #but we definetly want to exclude the moth 
      rectangle_sizes<-append (rectangle_sizes,
                              list(list(width=width,height=height,color=colors)))}
  }
  #-----------------------------------------------
  
#Step 17: DETERMINE LENGTH AND WIDTH OF ALL BOXES
##!!! my original code went off height, but as that is inaccurate I am going to go off width!!!
  rect_shortside<-list() #new lists

#Step 18: FIND SHORTEST SIDE 
  for (element in rectangle_sizes){
  #I want it to find the shorter side 
   short_side<-min(element$width,element$height) #find the min number from l or w
    rect_shortside <- append(rect_shortside, short_side)
  }

#---------------DETERMINE PIXEL TO CM CONVERSION BASED ON SHORTEST SIDE-------------------------------
#I keep getting an error abt "x must be atomic" apparently I have to unlist the varibles from the list?
  rect_shortside_unlisted <-unlist(rect_shortside)

#Step 19: FIND THE MEDIAN 
#I am not going to do the average, median is way better if there are outliers
  median_width<-median(rect_shortside_unlisted)
  print(median_width) #so median is 41 px
  #-----------------------------------------------

#Step 20: CONVERSION
  px_to_cm= 0.5/median_width # I need to ask joe what the width of the box is 
#0.5cm is the width of the box
  print(px_to_cm) #<--ok it looks good
  #-----------------------------------------------

#Step 21: APPLY TO MOTH
  cordinates_cm <- cordinates_Polygon * px_to_cm
#print(cordinates_cm)
#If I find this to be inaccurate we will change it back to what it was originally
#(which was the color scale)
  moth_width_cm <- max(cordinates_cm[,"x"]) - min(cordinates_cm[,"x"])
  moth_height_cm <- max(cordinates_cm[,"y"]) - min(cordinates_cm[,"y"])


  plot (cordinates_Polygon,main = sprintf("Moth width: %.2f cm\nMoth height: %.2f cm\n", 
                              moth_width_cm, moth_height_cm))#<-suprisingly helpful to visualize

  #---------------FIND MOTH PERCENT COLOR-------------------------------
#lets get color percentage out of the way real quick:)
# I already have the colors figured out so lets just use the old code :)

#chosen_one<--my polygon 
#Imagex, Imagey <-- the size of my picture

#I think I need to make a true/false so like inside polygon =true outside=false 
  
#Step 21: CONVERT TO HSV                                                                     #Potential improvement (color correction)                       
  moth_hsv <- RGBtoHSV(moth) #using the moth image and not blurred so I can get the exact pixels
  H <- channel(moth_hsv,1)
  S <- channel(moth_hsv,2)
  V <- channel(moth_hsv,3)
  #-----------------------------------------------
  
#Step 22: COLOR RANGES
#https://www.youtube.com/watch?v=m5WkLhjLqLo 
#https://www.hslpicker.com/#ffffff <--Saved my lifeeee
#hehe I can just use the ranges I used for the colorbar >:) 
  white_moth <- ((S < 0.2)& (V > 0.6))
  black_moth <- (V < 0.2)

#double parenthisies cause I want it all to be included B)   (glad I caught that error)
  red_moth <- (((H >= 0 & H <= 10) | (H >= 300 & H <= 340)) & (S > 0.2) & (V > 0.2))
  orange_moth <- ((H > 10) & (H < 40)& (S > 0.2)& (V > 0.2))
  yellow_moth <- ((H > 40) & (H < 70) & (S > 0.2)& (V > 0.2))
  green_moth <- ((H > 70) & (H < 165) & (S > 0.2) &(V > 0.2))
  lightblue_moth <- ((H > 165) & (H < 190) & (S > 0.2) &(V > 0.2))
  blue_moth <- ((H > 190) & (H < 260) & (S > 0.2)& (V > 0.2))
  purple_pink_moth<-((H > 260) & (H < 300) & (S > 0.2) &(V > 0.2))

  grey_moth <- ((H > 0) & (H < 360)& (S < 0.2) & (S > 0.1) & (V > 0.25) & (V < 0.75))
  brown_moth <- ((H > 20) & (H < 45) & (S > 0.2) & (V > 0.2) & (V < 1))
  #-----------------------------------------------

#Step 23: MAKE A BINARY MASK USING RASTER
  #originally I had a raster layer with true inside polygon and false outside 
  #https://www.geeksforgeeks.org/r-language/combine-a-polygon-map-on-top-of-a-raster-map-in-r/
  
  library(raster)
 #so first we want a raster layer
  #I want it to fit my moth image :)
  raster_map <- raster(nrows = Imagex, 
                       ncols = Imagey, 
                       xmn = 0, #Left
                       xmx = Imagex,  #Right
                       ymn = 0,  #Bottom
                       ymx = Imagey    #top
                       )
  #print(raster_map)<--looks good4
  #I want to make the polygon a raster...I think
  #chosen_one <-- polygon
  #plot(chosen_one)<--ok cute 
#https://www.rdocumentation.org/packages/raster/versions/3.6-32/topics/rasterize
#transfer values associated with the geometries of vector data to a raster
  Mm<-rasterize(chosen_one, raster_map,field=1,background=0)
  plot(Mm) #YAY
#head(Mm)<-----Basicalls 1 2 3
#   =                  1 0  1 0
  #-----------------------------------------------
  
#Step 24: LOOK FOR PIXELS THAT FALL IN THE COLOR RANGES AND MASK 
  white  <- (white_moth)&  (Mm[] == 1) #Mm[]<--looks at the raster cell values (NUMERIC)
  black  <- (black_moth)&  (Mm[] == 1) #If I just do Mm<--it would look at full raster
  red    <- (red_moth)  &  (Mm[] == 1) #and give error numeric, logical or complex types
  orange <- (orange_moth)& (Mm[] == 1)
  green  <- (green_moth)&  (Mm[] == 1)
  lightblue <- (lightblue_moth)& (Mm[] == 1)
  blue   <- (blue_moth)& (Mm[] == 1)
  yellow <- (yellow_moth)& (Mm[] == 1)
  grey   <- (grey_moth)& (Mm[] == 1)
  brown  <- (brown_moth)& (Mm[] == 1)
  purple_pink <- (purple_pink_moth)& (Mm[] == 1)
  #-----------------------------------------------
  
#Step 24: FIND THE SUM 
#ok now sum that baby up
#cutsey little dataset of all the colors :)
  colorpercentageee <- data.frame(
    Colornames = c("White","Black","Red","Orange","Green","Light Blue","Blue","Yellow","Grey","Brown","Purple/pink"),
    Amount = c(
      sum(white),
      sum(black),
      sum(red),
      sum(orange),
      sum(green),
      sum(lightblue),
      sum(blue),
      sum(yellow),
      sum(grey),
      sum(brown),
      sum(purple_pink))
  )
  Total<- (sum(white)+sum(black)+sum(red)+sum(orange)+sum(green)+sum(lightblue)+sum(blue)+sum(yellow)+sum(grey)+sum(brown)+sum(purple_pink))
  print (Total)
#131533

  colorpercentageee$Percent<-(colorpercentageee$Amount/Total)*100

  yaydone <- colorpercentageee$Colornames[which.max(colorpercentageee$Percent)]    #Potential improvement (maybe should be top 3...)   
#  print(yaydone)
  #-----------------------------------------------

#I am going to make this in a separate file so after you get your csv
#now you can run my MOMOCS code
  shape<-"triangle"

#---------------MetaDATA-------------------------------
#https://www.youtube.com/watch?v=SCT4o4vz97o                                 #Potential improvement (if statement,so if you don't install that you can still run it )  
#literally just watched videos on how to do this :)
  #https://www.youtube.com/watch?v=Ku1Nx-kl7RM
  #follow comment thats says" For those who are doing this a few years later and are running 
  #into the "Could not find C:\Program Files (x86)\Exiftool\exiftool_files\perl5*.dll" or something 
  #similar, just remember to also copy over the exiftool_files folder from the download."
  
  #https://cran.r-project.org/web/packages/exifr/index.html<--YOU NEED TO DOWNLOAD THIS
  moth<-mURL #get the URL
  download.file(moth, "temp_moth.png", mode = "wb") #Tenporarily download it :)
  file.exists("temp_moth.png")
  
  library("exifr") #allows us to see metadata
  configure_exiftool("C:/Program Files/Exiftool/exiftool.exe") 
  metadata<-read_exif("temp_moth.png")
  
  timetaken<-metadata$DateTimeOriginal
  timetaken<-ifelse(is.null(timetaken),0,timetaken) #the should save the NULL as 0 :)
  #https://www.educative.io/blog/what-is-isna-function-in-r
  #my value was null tho not na
  
  filedate<-metadata$FileCreateDate #could be saved as this as well
  filedate<-ifelse(is.null(filedate),0,filedate)
  
  locationx<-metadata$GPSLatitude 
  locationx<-ifelse(is.null(locationx),0,locationx)
  
  
  locationy<-metadata$GPSLongitude
  locationy<-ifelse(is.null(locationy),0,locationy)
  
  #METADATA CAN BE ADDED PRETTY EASY NOW!!! <--If I want to use other info at some point 
  #-----------------------------------------------
  
  #GATHER UP all the info for a new row!!!
  new_row<- data.frame(
    Moth_URL=mURL,
    Color_URL=cURL,
    Width=moth_width_cm,
    Height=moth_height_cm,
    Color=yaydone,
    Shape=shape,
    File_Date=filedate,
    Time_Taken=timetaken,
    Location_X=locationx,
    Location_Y=locationy
  )
  results_CSV<-rbind(results_CSV,new_row)
  
  }#<----this is that csv loop
write.csv(results_CSV,"Results_Moth.csv")
