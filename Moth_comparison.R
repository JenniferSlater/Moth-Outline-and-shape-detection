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
  
  translate$X <- as.numeric(translate$X)
  translate$Y <- as.numeric(translate$Y)
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
Lovley_moths[[1]] #YAYYYYY
#Ok now I want to use the OUT object
#this is the fac value that lets me seperate the outlines :)
Moth_ID_List<-data.frame(unique(translate$ID)) #I think it needs to be a dataframe

Cute_moth_outlines<-Out(Lovley_moths, Moth_ID_List)
#list(translate$ID) #I want to remove the repeating numbers

#Lets just see them real quick 

stack(Lovley_moths, fac=Moth_ID_List)
