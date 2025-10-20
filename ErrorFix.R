#https://justin-liang.com/tutorials/canny/ <--this should help
library(imager)

#"C:/Users/slate/Desktop/Anavitrinella_pampinaria_727.JPG"
#"C:/Users/slate/Pictures/Anavitrinella_pampinaria_727 - Copy (2).JPG
#"C:/Users/slate/Pictures/Cropped3.png"
#CroppedXD.JPG
mothp <-load.image("C:/Users/slate/Desktop/Eueretagrotis_sigmoides_629.JPG")
moths <- imresize(mothp,scale=0.5)

#first thing to do it plot the moth to know what we dealing with :)
plot(moths,main="The original moth")
#Step 1: from Canny edge detection (turn to greyscale)

#issue with greyscale so lets test the dim to see whats up
dim(moths)        # w h     color channel   frames
#the moth image is ## ##    1(should be 3)    #
#so there is only 1 meaning there must have been something that mucked it up
#(x,y,c<--colorchannel,z<--frames)

#I found un edited images work so that is my workaround for the moment
#BUG FIX EVENTUALLY------------------------------

#step 1 greyscale--------------------------------------------------------------

gmoth <- grayscale(moths, drop = TRUE)
plot(gmoth, main="Greyscale of Moth") 

#Step 2 Gaussian Blur <--I found a way to set in imager :)
bmoth<-isoblur(gmoth,4,neumann = TRUE, gaussian = TRUE, na.rm = TRUE)

plot(bmoth, main="Blurred Moth") 

#step 2.5 side quest!!!
#I tried gradient magnitude by itself and it was thicker but more accurate
#maybe overlapping could work?
dx <- imgradient(bmoth,"x") 
dy <- imgradient(bmoth,"y")

grad.mag <- sqrt(dx^2+dy^2) 
plot(grad.mag,main="Gradient magnitude Side Quest")

gmoth<-isoblur(grad.mag,2,neumann = TRUE, gaussian = TRUE, na.rm = FALSE)
plot(gmoth,main="Gradient magnitude")

#step 3 Uncanny
Uncanny1 <- cannyEdges(bmoth, sigma=1.5)
plot(Uncanny1, main="CannyEdges normal")

Uncanny2 <- cannyEdges(gmoth, sigma=1)
plot(Uncanny2, main="CannyEdges with gm")
##NORMAL WINS!!!

#step 4
#ok lets plot it
coords <- which(Uncanny1, arr.ind = TRUE)
wing_points2 <- data.frame(
  yy2=coords[,1], #this will be my y corrd/row
  xx2=coords[,2] #this will be my y coord/colum
)
library(ggplot2)
#ok so now we have the data, lets make a scatter plotttt
ggplot(wing_points2, aes(x=xx2, y=yy2)) + 
  
  geom_point(
    color="blue",
    size=0.5)+ #make it pretty:)
  coord_fixed()+
  coord_flip()+
  scale_x_reverse()+
  ggtitle("moth ploted Canny Edges no reduction:))")


#I think I could hypothetically convert this to a matrix here but idk
matrixz <- as.matrix(Uncanny1)
print(matrixz)
#bruhhhh its a Travelling salesman problem TwT (says google) 
#lets put it in a data frame
df <- as.data.frame(Uncanny1)
print(df)
#we only want the edge points but that is not something I can really do
#BUT we PERSIST ON!!!
head(df)
colnames(df) #to check :)

#step 5 CONNECT THE DOTSSS
gimmeapoint<- as.matrix(df[,c("x","y")]) #<-dataframe, I do need to convert to matrix
#x,y,z,cc
#I only want it to give me x and y
  #subset is basically extract points
print(gimmeapoint)
#cool  it is looking good
library(tidyverse) #gonna mess with data so prolly a good idea
library(FNN) #Fast Nearest Neighbor Search <--fingers crossed this is better

#lets try this traveling sales man problem
#what I want it to do...
#take a point and visit it
#Remove it from the dataset

#Its just like python, lets start with an unvisited dataset
unvisited<-gimmeapoint #lateron I will subtract from this...maybe 
#if it works
#lets start with the first points cause the moth I am using is tipped 
#...and its convenient
path<-unvisited[1,,drop=FALSE]
unvisited<-unvisited[-1,,drop=FALSE]#<--everything else

while(nrow(unvisited)>0){ #<--while there are still points in unvisited
  mostrecentpt<-(tail(path,1))#<adds 1

  #lets calculate the distance its in the loop so it does it everytime  
  #my last attemt took to long since it was calculating every possibility
  letstrynewlibrary<- get.knnx(data=unvisited,query=mostrecentpt,k=1, algorithm = "kd_tree")
  #I may try out diffrent algorithms...DO NOT TRY COVER TREE IT CRASHED SO BADDD!!!
  # get.knnx(data, query, k=10, algorithm=c("kd_tree", "cover_tree", "CR", "brute"))
  
  nearestpoint <- letstrynewlibrary$nn.index[1]
  test<-letstrynewlibrary$nn.dist[1]

  if (test>150){
      maybenot<- nearestpoint 
      unvisited<-unvisited[-nearestpoint,,drop=FALSE]}
#Adding an if then statement to help me out :)
#if it is a distance longer than 250 pixels, just get rid of it :)
#130 seems like the minimum from what I can tell
#going to 150 cause it works and there is prolly variation
  else{
  
  #PUT IT ALL TOGETHER!!
    path<-rbind(path,unvisited[nearestpoint,,drop=FALSE])
  #bind_rows does not work for matrices apparently
    unvisited<-unvisited[-nearestpoint,,drop=FALSE]
    head(unvisited)#<just to make sure its good
}}

ggplot(path,aes(x=x,y=y))+
  geom_path(color = "blue") +
  theme_minimal()

#AHHHHH IT WORKSSS!!!!

#TODO WHAT I NEED TO ADD
#- Better edge detection to not get patterns in wings
#- filling in the moth shape

