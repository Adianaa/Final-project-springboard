# Installing the required packages in R for this project
library(recommenderlab)
library(reshape2)
library(ggplot2)
# Read training file along with header
tr <- read.csv("C:/Users/Anamikja/Desktop/Anamika/Springboard/train_v2.csv", header = TRUE)

head(tr)
# Removing  'id' column since it is not needed
tr<-tr[,-c(1)]

tr[tr$user==1,]
# re working the data to resemble a matrix like structure
g<-acast(tr, user ~ movie)
class(g)

# Converting g into an actual matrix
R<-as.matrix(g)

# Converting R into realRatingMatrix data structure
#   realRatingMatrix is a recommenderlab sparse-matrix like data-structure
r <- as(R, "realRatingMatrix")
r

# normalizing the rating matrix
r_m <- normalize(r)
r_m
# viewing it as a list
as(r_m, "list")

# plotting an image plot of raw-ratings & normalized ratings
#  A column represents one specific movie and ratings by users
#   are shaded.
#   Note that some items are always rated 'black' by most users
#    while some items are not rated by many users
#     On the other hand a few users always give high ratings
#      as in some cases a series of black dots cut across items
image(r, main = "Raw Ratings")       
image(r_m, main = "Normalized Ratings")

# Creating a recommender object (model)
#        UBCF: User-based collaborative filtering
#      Parameter 'method' decides similarity measure
#       Jaccard method of distance measuring is implemented
rec=Recommender(r[1:nrow(r)],method="UBCF", param=list(normalize = "Z-score",method="Jaccard",nn=5, minRating=1))
# Depending upon your selection, examine what you got
print(rec)
names(getModel(rec))
getModel(rec)$nn

############Create predictions#############################
# This prediction does not predict movie ratings for test.
#   But it fills up the user 'X' item matrix so that
#    for any userid and movieid, I can find predicted rating

#      'type' parameter decides whether you want ratings or top-n items
#       To  get top-10 recommendations for a user :
#       predict(rec, r[1:nrow(r)], type="topNList", n=10)
recom <- predict(rec, r[1:nrow(r)], type="ratings")
recom

########## Examination of model & experimentation  #############


# Converting prediction into list, user-wise
as(recom, "list")
# Study and Compare the following:
as(r, "matrix")     # Has lots of NAs. 'r' is the original matrix
as(recom, "matrix") # Is full of ratings. NAs disappear
as(recom, "matrix")[,1:10] # Show ratings for all users for items 1 to 10
as(recom, "matrix")[5,3]   # Rating for user 5 for item at index 3
as.integer(as(recom, "matrix")[5,3]) # Just to get the integer value
as.integer(round(as(recom, "matrix")[6039,8])) # Just to get the correct integer value
as.integer(round(as(recom, "matrix")[368,3717])) 

# Convert all your recommendations to list structure
rec_list<-as(recom,"list")
head(summary(rec_list))
# Access this list. User 2, item at index 2
rec_list[[2]][2]
# Convert to data frame all recommendations for user 1
u1<-as.data.frame(rec_list[[1]])
attributes(u1)
class(u1)
# Create a column by name of id in data frame u1 and populate it with row names
u1$id<-row.names(u1)
# Check movie ratings are in column 1 of u1
u1
# Now access movie ratings in column 1 for u1
u1[u1$id==3952,1]

########## Create final File from model #######################
# Read test file
test<-read.csv("C:/Users/Anamikja/Desktop/Anamika/Springboard/test_v2.csv",header=TRUE)
head(test)
# Get ratings list
rec_list<-as(recom,"list")
head(summary(rec_list))
ratings<-NULL
# For all lines in test file, one by one
for ( u in 1:length(test[,2]))
{
  # Read userid and movieid from columns 2 and 3 of test data
  userid <- test[u,2]
  movieid<-test[u,3]
  
  # Get as list & then convert to data frame all recommendations for user: userid
  u1<-as.data.frame(rec_list[[userid]])
  # Creating a (second column) column-id in the data-frame u1 and populate it with row-names
  # We use row.names() function
  u1$id<-row.names(u1)
  # Now access movie ratings in column 1 of u1
  x= u1[u1$id==movieid,1]
  # print(u)
  # print(length(x))
  # If no ratings were found, assign 0. 
  if (length(x)==0)
  {
    ratings[u] <- 0
  }
  else
  {
    ratings[u] <-x
  }
  
}
length(ratings)
tx<-cbind(test[,1],round(ratings))
# Write to a csv file
write.table(tx,file="Finalfile.csv",row.names=FALSE,col.names=FALSE,sep=',')

