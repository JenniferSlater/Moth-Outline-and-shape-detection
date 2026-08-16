library(tidyr)
library(Momocs)
moths <- read.csv("C:/Users/slate/Documents/Morphometric_moth_results.csv", header=TRUE)

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
panel(Cute_moth_outlines) #some points look rough but honestly it looks pretty nice!
#Cute_moth_outlines[1] %>% coo_plot() #cool!

#invalid "xlim" value 
#The error apparently can happen if it is "Non-Numeric Data"

#So I cannot use the proctustes b/c there are no landmarks!
stack(Cute_moth_outlines)

#https://momx.github.io/Momocs/reference/efourier.html
#coo <- Cute_moth_outlines[2]
#coo_plot(coo)
#ef <- efourier(coo, 12)
#efourier(coo, 12, norm=TRUE)
#efi <- efourier_i(ef)
#coo_draw(efi, border='red', col=NA)

pls_work<- efourier(Cute_moth_outlines,nb.h= 12)

#nb.h=The number of harmonics to use
#smooth.it=The number of smoothing iterations to perform
#norm= whether or not to normalize the coefficents \
#- if they look upsidedown, align the outlines b4 hand and turn norm as false
#start= consider the first point to all be the same 

tunnel_light<-PCA(pls_work)
tunnel_light
plot(tunnel_light) #OMG IT WORKEDDD

#ok but BUT I think we have a problem, unlike the bottle image these moths are facing diffrent directions...
#I should prolly fix this... but HOW 

#Kmeans!
