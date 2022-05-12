# clear any saved variables in R
rm(list=ls())

#set a working directory
setwd("C:/your_directory_here")

#read in a csv of your data. 
#your csv should consist of rows that are the kill sites (use points) and random points (non use points)
#the columns will be the data for each variable of interest at that point (slope, elevation, etc)
#for simplicity, give each column a brief, lowercase name with no spaces in it
#Make sure you don't have any empty columns and everything is formatted correctly
your.data <- read.csv("your_csv_of_data.csv", header = T)


#scale the data to make sure all the coefficients are comparable, and to speed up computation
#scale the numerical columns of interest. Not everything needs to be scaled. Lat/long, for example, or any categorical (ie, non-numerical) variables don't need to be scaled
# the use/non-use data should not be scaled - make sure that is all 1's and 0's. Info below on how to add that back in
scaled.your.data <- scale(your.data[1:40]) #enter the appropriate column numbers for your data that you want to scale

#save the scaling info to be used later
scaleList <- list(scale = attr(scaled.your.data, "scaled:scale"),
                  center = attr(scaled.your.data, "scaled:center")) 

#add back any columns that you didn't scale. Here is an example for how to do that from some old code of mine
#this is the data frame you will use for modeling
final.data <- data.frame("sort_code" = your.data$sort_code, "attack" = your.data$attack, 
                        "kill_year" = your.data$kill_year, "protection" = your.data$protection, 
                        scaled.your.data)


############### model building

#create a null model
#here the variable of interest is your use/nonuse points. I called that variable "kill" here
null.model <- glm(kill ~ 1, family = binomial, data=final.data)
summary(null.model) #take note of the AIC

#create a full model with all relevant variables
#change 'column1', 'column2' etc to the column names for the predictor variables (eg, distance to road, elevation, etc. Make sure they are the column names from your csv)
full.model <- glm(kill ~ column1 + column2 + column3 + ... , 
                  family = binomial, data = final.data, na.action = "na.fail")
#note that if you have any NAs in your code, that will cause issues! 
#in some cases you'll want to change those NA's to 0's, where it makes sense to do so. 
#If the NAs reflect an absence of data, you may have to remove those rows from your CSV

summary(full.model) #take note of the AIC

#If you get this far, you'll be able to directly compare the estimates for all your numeric variables
#The larger the absolute value of the estimate, the bigger the influence on kill site
#positive values mean that variable is positively correlated with kill site, negative is the opposite

#You can stop here and have a great project! Just repeat for the two phases and you're done
#But if this was all really easy, the next step is to do model selection.
#I can explain more, but a few lines of code will get you pretty far
#First, you want to check if any of your predictor variables are collinear. You can do this with the VIF (variance inflation factor)
vif(full.model)
#if any values are greater than 4, you have collinearity and need to make a decision about excluding one of those variables

#next you can use the dredge function to do model selection: 
dredge(full.model)
#this will output a table of estimates and AIC values. We can talk about how to interpret this if you get this far

#all of this really just tells you what the best model is based on the variables you've chosen
#but is it a really a good model, objectively? You can use the area under teh receiver operating characteristics curve to test this. 
#Here is some code to bootstrap that calculation
#make sure to replace the sample model with your model values (line 77-78)

auc.data <- matrix(0, ncol = 1, nrow = 100)
auc.frame <- data.frame(auc.data)

x <- c(1:100)
for(i in c(1:100)){
  Train <- createDataPartition(final.data$kill, p=0.8, list=FALSE) #split the data set, using 80% of the sample for training
  training <- final.data[ Train, ] #make training
  testing <- final.data[ -Train, ] #make testing
  
  full.model.train <- glm(kill ~  column1 + column2 + column3 + ... , 
                          family = binomial, data = final.data, na.action = "na.fail")
  # Compute AUC for predicting Class with the full model
  #AUC above .7 is good, above .8 is excellent fit
  prob <- predict(full.model.train, newdata=testing, type="response", allow.new.levels = TRUE)
  pred <- prediction(prob, testing$attack)
  perf <- performance(pred, measure = "tpr", x.measure = "fpr")
  
  auc <- performance(pred, measure = "auc")
  auc <- auc@y.values[[1]]
  auc
  auc.frame$auc.frame[i] <- auc
}

mean(auc.frame$auc.frame) # this gives you the mean value. Above 0.7 is good, above 0.8 is very good
range(auc.frame$auc.frame) #the range of values
sd(auc.frame$auc.frame) #the sd of values

#If you've gotten this far, that's amazing, and we can talk about next steps which might include:
#--Mapping a risk surface for the whole area
#--Comparing this surface with elk vigilance

