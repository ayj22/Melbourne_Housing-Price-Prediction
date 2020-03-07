
#HOUSING MARKET ANALYSIS FOR MELBOURNE CITY.
install.packages("corrplot")
install.packages("rpart.plot")
install.packages("tree")
install.packages("Metrics")
install.packages("randomForest")
install.packages("dummies")
install.packages("ggcorrplot")
install.packages("VIF")
install.packages("car")
install.packages("corrplot")
install.packages("gvlma")
install.packages("MASS")
install.packages("dplyr")
install.packages("stringr")
install.packages("metrics")

library(stringr)
library(dplyr)
library(tidyr)
library(dummies)
library(ggcorrplot)
library(corrplot)
library(VIF)
library(MASS)
library(car)
library(rpart.plot)
library(tree)
library(Metrics)
library(randomForest)
library(gvlma)

options(scipen=999)  #removing scientific notations.
options(max.print=1000000)

#mel <- read.csv(file.choose())     
mel <- read.csv(file.choose())

#Undertsanding the nature and specifications of data

#Exploring the data
my_missing_NA_value_function <- function(dataset){
  
  Total_NA <- sum(is.na(dataset))
  Column_sums <- colSums(is.na(dataset))
  cat("Total NA in the dataset in all in the columns- \n\n",Total_NA)
  cat("\n--------------##-----------------")
  Column_names <- colnames(dataset)[apply(dataset,2,anyNA)]
  cat('\n\n Names of NA columns in the dataset-\n\n',Column_names)
  cat('\n\n Total NA by column in the dataset-\n\n',Column_sums)
  cat("\n--------------##-----------------")
}

my_data_overview <- function(dataset){
  data <- dim(dataset)
  cat("\nTotal Number of [rows vs columns] in the dataset- \n",data)
  cat("\n--------------##-----------------")
  Column_datatypes <- sapply(dataset,class)
  cat('\n\n Datatypes of all the columns in the dataset-\n',Column_datatypes)
  cat("\n--------------##-----------------")
  Column_Names <- colnames(dataset)
  cat('\n\n Names of all the columns in the dataset-\n',Column_Names)    
}

my_data_overview(mel)
my_missing_NA_value_function(mel)

########################## Correction of Data types ########################
sapply(mel, class)   #checking datatypes of all column in dataset
mel$Address <- as.character(mel$Address)    #changing dataset from factor to character
mel$Rooms <- as.factor(mel$Rooms)   #changing dataset from integer to factor
mel$Distance<- as.integer(mel$Distance)   #changing dataset from factor to integer
mel$Bathroom <- as.factor(mel$Bathroom)    #changing dataset from integer to factor
mel$Car <- as.numeric(mel$Car)            #changing dataset from integer to factor
mel$YearBuilt <- as.factor(mel$YearBuilt)   #changing dataset from integer to factor

sapply(mel, class)   #Re-checking datatypes of all column in dataset

######################### NA's Count in each column ########################
colSums(is.na(mel))

#Remove NA values of Price its dependent variable
mel1 <- subset(mel,(!is.na(mel[,5])))
colSums(is.na(mel1))
dim(mel1)

#Remove BuildingArea Column as it consist more then 60% of NA values
mel2 <- mel1[,c(1:14,16:21)]
colSums(is.na(mel2))
dim(mel2)

#Remove Latitude & Longitude Column as it consist more then 20% of NA values and we already have the suburban and region columns.
#mel4 <- mel2[ ,c(1:16, 19:20)]
#colSums(is.na(mel4))
#dim(mel4)
mel4 <- mel2
#almost 61% of the data for the rooms and Bedrooms is same i.e example if rooms==2 then bedroom2 ==2
temp <- mel4[,c("Rooms","Bedroom2")]
temp[which(temp$Rooms == temp$Bedroom2),]

#assigning the NA's of Bedrooms with the values of rooms.
my.na <- is.na(mel4$Bedroom2)
mel4$Bedroom2[my.na] <- mel4$Rooms[my.na]

colSums(is.na(mel4))

########################## Outliers ############################

#Checking the outliers
#par(mar=c(3.1,12,4.1,2.1), mgp = c(11, 1, 0))

boxplot(mel4$Price ~ mel4$Regionname, horizontal = TRUE, ylab = "REGION NAME", xlab = "PRICE OF HOUSE", main = "BOXPLOT: PRICE OF HOUSE BY REGION", las = 1)

#par(mar=c(3.1,12,0.95,2.1), mgp = c(11, 1, 0), mfrow = c(2,1))

boxplot(mel4$Landsize ~ mel4$Regionname, horizontal = TRUE, xlab = "Landsize of houses", ylab = "Region Name", main = "BOXPLOT OF LANDSIZE OF HOUSES BY REGION", las = 1)

boxplot(mel4$Distance ~ mel4$Type, horizontal = TRUE, ylab = "Type of House", xlab = "Distance from CBD", main = "Boxplot of distance from CBD vs type of houses", las = 1)

#Checking outliers based on summary
boxplot(mel4$Price,main = "PRICE OF HOUSES.")

mel4$Rooms <- as.integer(mel4$Rooms)
boxplot(mel4$Rooms,main = "NUMBER OF ROOMS.")

class(mel4$Bedroom2)
boxplot(mel4$Bedroom2,main = "NUMBER OF BEDROOMS.")
boxplot(mel4$Landsize,main="LANDSIZE OF HOUSES.")

#Removing outlier from Rooms column
outliers <- boxplot(mel4$Rooms, plot=FALSE)$out
mel4[which(mel4$Rooms %in% outliers),]
mel4 <- mel4[-which(mel4$Rooms %in% outliers),]
boxplot(mel4$Rooms)

#Removing outlier from Bedroom2 column
outliers <- boxplot(mel4$Bedroom2, plot=FALSE)$out
mel4[which(mel4$Bedroom2 %in% outliers),]
mel4 <- mel4[-which(mel4$Bedroom2 %in% outliers),]
boxplot(mel4$Bedroom2)

#Removing outlier from Landsize column
outliers <- boxplot(mel4$Landsize, plot=FALSE)$out
mel4[which(mel4$Landsize %in% outliers),]
mel4 <- mel4[-which(mel4$Landsize %in% outliers),]
boxplot(mel4$Landsize)

#Review Dataset Details
colSums(is.na(mel4))

#landsize column fixation
#Making new dataframe of Bedrooms & Landsize
bed.land.df <- mel4[,c("Bedroom2","Landsize")]

unique(bed.land.df$Bedroom2)
colSums(is.na(bed.land.df))
bed.land.df <- na.omit(bed.land.df)
bed.land.df <- bed.land.df[which(bed.land.df$Landsize > 0),]

colSums(is.na(bed.land.df))
bed.land.df_0 <- bed.land.df[which(bed.land.df$Bedroom2 == 0),]
bed.land.df_1 <-  bed.land.df[which(bed.land.df$Bedroom2 == 1),]
bed.land.df_2 <- bed.land.df[which(bed.land.df$Bedroom2 == 2),]
bed.land.df_3 <-  bed.land.df[which(bed.land.df$Bedroom2 == 3),]
bed.land.df_4 <- bed.land.df[which(bed.land.df$Bedroom2 == 4),]
bed.land.df_5 <-  bed.land.df[which(bed.land.df$Bedroom2 == 5),]
bed.land.df_6 <- bed.land.df[which(bed.land.df$Bedroom2 == 6),]
bed.land.df_7 <-  bed.land.df[which(bed.land.df$Bedroom2 == 7),]

#Replacing Na values with 0
mel4$Landsize[which(is.na(mel4$Landsize))] <- 0

#120 logic is used here under the assumption that minimun sq feet required is 
#120 mtrs according to the https://www.godownsize.com/minimum-house-square-footage/

mel4$Landsize[which(mel4$Landsize < 120)] <- 0

#Replacing 0 values with median values
mel4$Landsize[which(mel4$Bedroom2 == 0 & mel4$Landsize== 0)] <- median(bed.land.df_0$Landsize[which(bed.land.df_0$Landsize > 1)]) 
mel4$Landsize[which(mel4$Bedroom2 == 1 & mel4$Landsize== 0)] <- median(bed.land.df_1$Landsize[which(bed.land.df_1$Landsize > 1)]) 
mel4$Landsize[which(mel4$Bedroom2 == 2 & mel4$Landsize== 0)] <- median(bed.land.df_2$Landsize[which(bed.land.df_2$Landsize > 1)]) 
mel4$Landsize[which(mel4$Bedroom2 == 3 & mel4$Landsize== 0) ] <- median(bed.land.df_3$Landsize[which(bed.land.df_3$Landsize > 1)]) 
mel4$Landsize[which(mel4$Bedroom2 == 4 & mel4$Landsize== 0) ] <- median(bed.land.df_4$Landsize[which(bed.land.df_4$Landsize > 1)]) 
mel4$Landsize[which(mel4$Bedroom2 == 5 & mel4$Landsize== 0) ] <- median(bed.land.df_5$Landsize[which(bed.land.df_5$Landsize > 1)]) 
mel4$Landsize[which(mel4$Bedroom2 == 6 & mel4$Landsize== 0) ] <- median(bed.land.df_6$Landsize[which(bed.land.df_6$Landsize > 1)]) 
mel4$Landsize[which(mel4$Bedroom2 == 7 & mel4$Landsize== 0) ] <- median(bed.land.df_7$Landsize[which(bed.land.df_7$Landsize > 1)]) 

#Checking if all the value got atleast 100 those are zero
mel4$Landsize[which(mel4$Landsize < 100)]
summary(mel4)

colSums(is.na(mel4))

#Car Column
#Putting median in all the NA values of Car column
mel4$Car <- as.numeric(mel4$Car)
mel4$Car[is.na(mel4$Car)] <- median(mel4$Car[which(!is.na(mel4$Car))])
colSums(is.na(mel4))

#Putting 0 in all the NA values of YearBuilt column
mel4$YearBuilt <- as.numeric(mel4$YearBuilt)
mel4$YearBuilt[which(is.na(mel4$YearBuilt))] <- 0
colSums(is.na(mel4))

colSums(is.na(mel4))

#Bathrooms column fixation
#ideally according to the https://hoa.org.uk/2017/05/room-ratio/# for every house 
#it should be a ratio of 3:2 to maximise its value and desirability.

#So 
#1 - 1
#2 - 1
#3 - 2
#4 - 2.5
#5 - 3
#6 - 4
#7 - 4.5

mel4$Bathroom <- as.integer(mel4$Bathroom)
mel4$Bathroom[which(is.na(mel4$Bathroom) & mel4$Bedroom2== 1)] <- 1
mel4$Bathroom[which(is.na(mel4$Bathroom) & mel4$Bedroom2== 2)] <- 1
mel4$Bathroom[which(is.na(mel4$Bathroom) & mel4$Bedroom2== 3)] <- 2
mel4$Bathroom[which(is.na(mel4$Bathroom) & mel4$Bedroom2== 4)] <- 2.5
mel4$Bathroom[which(is.na(mel4$Bathroom) & mel4$Bedroom2== 5)] <- 3
mel4$Bathroom[which(is.na(mel4$Bathroom) & mel4$Bedroom2== 6)] <- 4
mel4$Bathroom[which(is.na(mel4$Bathroom) & mel4$Bedroom2== 7)] <- 4.5

summary(mel4)

mel4 <- mel4[which(mel4$CouncilArea != '#N/A'),]
colSums(is.na(mel4))
dim(mel4)

#reverting back the datatypes which were changed in order to calculate the median.
mel4$Car <- as.numeric(mel4$Car)
mel4$Bathroom <- as.factor(mel4$Bathroom)
mel4$YearBuilt <- as.factor(mel4$YearBuilt)
mel4$Rooms <- as.factor(mel4$Rooms)

colSums(is.na(mel4))

mel4 <- na.omit(mel4)
#------------
str(mel4)
#dropping unused levels from the dataframe.
mel4$Postcode <- droplevels(mel4$Postcode)
mel4$CouncilArea <- droplevels(mel4$CouncilArea)
mel4$Regionname <- droplevels(mel4$Regionname)
mel4$Propertycount <- droplevels(mel4$Propertycount)
str(mel4)

table(mel4$CouncilArea)
#-------------
colSums(is.na(mel4))
#converting RegionName into numeric
mel4$Regionname <- as.character(mel4$Regionname)
mel4$Regionname[mel4$Regionname == 'Eastern Metropolitan'] <- 1
mel4$Regionname[mel4$Regionname == 'Eastern Victoria'] <- 2
mel4$Regionname[mel4$Regionname == 'Northern Metropolitan'] <- 3
mel4$Regionname[mel4$Regionname == 'Northern Victoria'] <- 4
mel4$Regionname[mel4$Regionname == 'South-Eastern Metropolitan'] <- 5
mel4$Regionname[mel4$Regionname == 'Southern Metropolitan'] <- 6
mel4$Regionname[mel4$Regionname == 'Western Metropolitan'] <- 7
mel4$Regionname[mel4$Regionname == 'Western Victoria'] <- 8

#converting method into numeric
mel4$Method = as.character(mel4$Method)
mel4$Method[mel4$Method == 'PI'] <- 1
mel4$Method[mel4$Method == 'PN'] <- 2
mel4$Method[mel4$Method == 'S'] <- 3
mel4$Method[mel4$Method == 'SA'] <- 4
mel4$Method[mel4$Method == 'SN'] <- 5
mel4$Method[mel4$Method == 'SP'] <- 6
mel4$Method[mel4$Method == 'SS'] <- 7
mel4$Method[mel4$Method == 'VB'] <- 8
mel4$Method[mel4$Method == 'W'] <- 9

#converting type into numeric
mel4$Type <- as.character(mel4$Type)
mel4$Type[mel4$Type == 'h'] <- 1
mel4$Type[mel4$Type == 't'] <- 2
mel4$Type[mel4$Type == 'u'] <- 3

#transforming the Date Column into Day Month Year
mel4 <- mel4 %>% separate(Date,sep = "/",into = c("Day","Month","Year"))

#converting month into season.
#spring (March, April, May), 
#summer (June, July, August), 
#autumn (September, October, November) 
#winter (December, January, February).

mel4$Season <- mel4$Month
mel4$Season <- as.numeric(mel4$Season)
mel4$Season[which(mel4$Season == 3 | mel4$Season == 4 | mel4$Season == 5)] = "Spring"
mel4$Season[which(mel4$Season == 6 |mel4$Season == 7 | mel4$Season == 8)] = "Summer"
mel4$Season[which(mel4$Season == 9 | mel4$Season == 10 | mel4$Season == 11)] = "Autumn"
mel4$Season[which(mel4$Season == 12 | mel4$Season == 1 | mel4$Season == 2)] = "Winter"

mel4$Season <- as.character(mel4$Season)
mel4$Season[mel4$Season == 'Spring'] <- 1
mel4$Season[mel4$Season == 'Summer'] <- 2
mel4$Season[mel4$Season == 'Autumn'] <- 3
mel4$Season[mel4$Season == 'Winter'] <- 4

#correlation checking of data
my_corrdata <- mel4[,-c(1,2,7,18)]
colnames(mel4)
#converting the datacolumn into numeric
my_corrdata$Regionname <- as.numeric(my_corrdata$Regionname)
my_corrdata$Method <- as.numeric(my_corrdata$Method)
my_corrdata$Type <- as.numeric(my_corrdata$Type)
my_corrdata$Rooms <- as.numeric(my_corrdata$Rooms)
my_corrdata$Distance <- as.numeric(my_corrdata$Distance)
my_corrdata$Postcode <- as.numeric(my_corrdata$Postcode)
my_corrdata$Bedroom2 <- as.numeric(my_corrdata$Bedroom2)
my_corrdata$Bathroom <- as.numeric(my_corrdata$Bathroom)
my_corrdata$Car <- as.numeric(my_corrdata$Car)
my_corrdata$YearBuilt <- as.numeric(my_corrdata$YearBuilt)
my_corrdata$Day <- as.numeric(my_corrdata$Day)
my_corrdata$Month <- as.numeric(my_corrdata$Month)
my_corrdata$Year <- as.numeric(my_corrdata$Year)
my_corrdata$Propertycount <- as.numeric(my_corrdata$Propertycount)
my_corrdata$Season <- as.numeric(my_corrdata$Season)

sapply(my_corrdata, class)
corr <- round(cor(my_corrdata),1)
ggcorrplot(corr)

#normalization
normalize <- function(x){
  return ((x - min(x)) / (max(x) - min(x)))
}

mel4$Landsize <- normalize(mel4$Landsize)
mel4$Distance <- normalize(mel4$Distance)

mel4$Rooms <- as.numeric(mel4$Rooms)
mel4$Rooms <- normalize(mel4$Rooms)

mel4$Bathroom <- as.numeric(mel4$Bathroom)
mel4$Bathroom <- normalize(mel4$Bathroom)

mel4$Car <- as.numeric(mel4$Car)
mel4$Car <- normalize(mel4$Car)

mel4$Propertycount <- as.numeric(mel4$Propertycount)
mel4$Propertycount <- normalize(mel4$Propertycount)
colnames(mel4)

#one hot encoding and datatype corrections
mel4$Regionname <- as.factor(mel4$Regionname) #one hot needed
mel4$Method <- as.factor(mel4$Method) #one hot needed
mel4$Type <- as.factor(mel4$Type) #one hot needed 
mel4$Rooms <- as.factor(mel4$Rooms) 
mel4$Car <- as.numeric(mel4$Car)
mel4$Bedroom2 <- as.factor(mel4$Bedroom2) 
mel4$Bathroom <- as.factor(mel4$Bathroom) 
mel4$YearBuilt <- as.numeric(mel4$YearBuilt)
mel4$Day <- as.numeric(mel4$Day)
mel4$Month <- as.factor(mel4$Month) #as we have create a new variable season using Month we will not be using month
mel4$Propertycount <- as.character(mel4$Propertycount) 
mel4$Propertycount <- as.numeric(mel4$Propertycount)
mel4$Season <- as.numeric(mel4$Season) #one hot encoding needed

#one hot encoding of type
type_ <- factor(mel4$Type)
dumm <- as.data.frame(model.matrix(~type_)[,-1])
mel4 <- cbind(dumm,mel4)

#one hot encoding of Method
Method_ <- factor(mel4$Method)
dumm <- as.data.frame(model.matrix(~Method_)[,-1])
mel4 <- cbind(dumm,mel4)

##one hot encoding of region
#Region_ <- factor(mel4$Regionname)
#dumm <- as.data.frame(model.matrix(~Region_)[,-1])
#mel4 <- cbind(dumm,mel4)

#one hot encoding of season
Season_ <- factor(mel4$Season)
dumm <- as.data.frame(model.matrix(~Season_)[,-1])
mel4 <- cbind(dumm,mel4)

#Rooms_ <- factor(mel4$Rooms)
#dumm <- as.data.frame(model.matrix(~Rooms_)[,-1])
#mel4 <- cbind(dumm,mel4)

#Bathroom_ <- factor(mel4$Bathroom)
#dumm <- as.data.frame(model.matrix(~Bathroom_)[,-1])
#mel4 <- cbind(dumm,mel4)

#Car_ <- factor(mel4$Car)
#dumm <- as.data.frame(model.matrix(~Car_)[,-1])
#mel4 <- cbind(dumm,mel4)

mel4$CouncilArea <- str_replace_all(mel4$CouncilArea,c(" "="_"))
Council_ <- factor(mel4$CouncilArea)
dumm <- as.data.frame(model.matrix(~Council_)[,-1])
mel4 <- cbind(dumm,mel4)

#test_df <- mel4[,-c(40,41,42,43,44,45,47,48,51,54,55,56,58,59,60,61)]



colnames(mel)
#===================
colnames(mel4)

#removing the Rooms from the final dataset as we observed high collinearity between Bedroom2 and Rooms
test_df <- mel4[,-c(42,43,45,47,48,50,51,53,54,58,59,62,64)]
colnames(test_df)
#splitting the data into train and test
create_train_test <- function(data, size = 0.7, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <- 1: total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}

set.seed(123)
train_data <- create_train_test(test_df,train = TRUE)
test_data <- create_train_test(test_df,train = FALSE)

######################################################
#### ---------Linear Regression
######################################################

colnames(train_data)

#full model 
model_l <- lm(Price ~ .,data = train_data)
summary(model_l)
plot(model_l)
vif(model_l)
options(warn=-1)
predicted_ys <- predict(model_l,newdata=test_data)
observed_ys <- test_data$Price
SSE <- sum((observed_ys - predicted_ys) ^ 2)
SST <- sum((observed_ys - mean(observed_ys)) ^ 2)
r2 <- 1 - SSE/SST
r2


summary(model_l)

##############################################
###--------STEPWISE MODEL
###############################################

step_model <- stepAIC(model_l,direction="both",trace=1)
summary(step_model)
step_model$anova
options(warn=-1)
predicted_ys <- predict(step_model,newdata=test_data)
observed_ys <- test_data$Price
lm.SSE <- sum((observed_ys - predicted_ys) ^ 2)
lm.SST <- sum((observed_ys - mean(observed_ys)) ^ 2)
lm.r2 <- 1 - lm.SSE/lm.SST
lm.r2
vif(step_model)

############################################################
############-----------Lasso regression
############################################################

install.packages("glmnet")
install.packages("lava")
library("glmnet")
library("lava")

x <-model.matrix(Price~.,data=train_data)
x_train <- x[,-1]
y_train <- train_data$Price
crossval <-  cv.glmnet(x = x_train, y = y_train)
plot(crossval)
penalty <- crossval$lambda.min
penalty
fit1 <-glmnet(x = x_train, y = y_train, alpha = 1, lambda = penalty ) #estimate the model with that
c <- coef(fit1)
inds<-which(c!=0)
variables<-row.names(c)[inds]
variables<-variables[variables %ni% '(Intercept)']
variables
summary(fit1)

x_1 <-model.matrix(Price~.,data=test_data)
x_test <- x_1[,-1]
y_test <- test_data$Price

predicted_ys <- predict(fit1, s = penalty, newx = x_test)
observed_ys <- test_data$Price
lm.SSE <- sum((observed_ys - predicted_ys) ^ 2)
lm.SST <- sum((observed_ys - mean(observed_ys)) ^ 2)
lm.r2 <- 1 - lm.SSE/lm.SST
lm.r2

######################################################
#### ---------Decision Tree 
######################################################

#test_df <- mel4[,-c(17,18,19,20,22,23,25,26,33,34,35,37)]
#test_df$Postcode <- as.numeric(test_df$Postcode)
set.seed(123)
train_data <- create_train_test(test_df,train = TRUE)
test_data <- create_train_test(test_df,train = FALSE)

tree.model <- rpart(Price ~ .,data = train_data)
summary(tree.model)
rpart.plot(tree.model,type = 5,extra=101)

# prune the tree 
pfit<- prune(tree.model, cp=tree.model$cptable[which.min(tree.model$cptable[,"xerror"]),"CP"])
rpart.plot(pfit,type = 5,extra=101)

tree.pred <- predict(pfit,newdata = test_data)
dt.SSE <- sum((test_data[,"Price"]-tree.pred)^2)
dt.SST <- sum((test_data[,"Price"] - mean(test_data[,"Price"]))^2)
dt.r2 <- 1 - dt.SSE/dt.SST
dt.r2

#######################################################
#### ---------Random Forests
#######################################################

#set.seed(123)
test_df$Postcode <- as.character(test_df$Postcode)
test_df$Postcode <- as.numeric(test_df$Postcode)
train_data <- create_train_test(test_df,train = TRUE)
test_data <- create_train_test(test_df,train = FALSE)

rf <- randomForest(Price ~ .,data = train_data)
rf.pred <- predict(rf,newdata = test_data)
SSE.rf <- sum((test_data[,"Price"]-rf.pred)^2)
SST.rf <- sum((test_data[,"Price"] - mean(test_data[,"Price"]))^2)
r2.rf <- 1 - SSE.rf/SST.rf
r2.rf


colnames(mel)

