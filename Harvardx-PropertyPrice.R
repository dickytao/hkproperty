## ----setup, include=FALSE---------------------------------------------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  message = FALSE,
  warning = FALSE # eval = FALSE
)

# install tinytex for PDF latex generation
if(!require(tinytex)) tinytex::install_tinytex() 

options(scipen = 0, digits = 6)



## ----library setup,  include=FALSE, message=FALSE, warning=FALSE------------------------------------------------


# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(grid)) install.packages("grid", repos = "http://cran.us.r-project.org")
if(!require(cowplot)) install.packages("cowplot", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")

# Library for Random Forest Model with more than 53 levels (to process factors with more than 53 levels)
if(!require(Rcpp)) install.packages("Rcpp", repos = "http://cran.us.r-project.org")
if(!require(ranger)) install.packages("ranger", repos = "http://cran.us.r-project.org")

# Library for Extreme Gradient Boosting Model
if(!require(xgboost)) install.packages("xgboost", repos = "http://cran.us.r-project.org")

# Library for Principal Component Analysis Model
if(!require(pls)) install.packages("pls", repos = "http://cran.us.r-project.org")

# For using ggplot to plot the latitude and longitude 
if(!require(sf)) install.packages("sf", repos = "http://cran.us.r-project.org")

# To read the shapfile to show the base map of Hong Kong
if(!require(mapview)) install.packages("mapview", repos = "http://cran.us.r-project.org")

# To show correlation plot
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")


 
library(tidyverse)
library(caret)
library(data.table)

library(lubridate)
library(gridExtra)
library(ggplot2)
library(grid)
library(cowplot)
library(scales)
library(knitr)
library(kableExtra)

# Libraries for Prediction Models
library(Rcpp)
library(ranger)
library(xgboost)
library(pls)

# Library for latitude and longitude display in ggplot
library(sf)

# Library to import shapefile and display base map boundary 
library(mapview)

# Library to show the correlation plot
library(corrplot)


## ----Map Data Download, echo=FALSE, include=FALSE, warning=FALSE------------------------------------------------

#######################################################################
##     Download Hong Kong map shapefile from github repositories     ##
#######################################################################

# A script to stop the need of file download if it exists in working folder
if(!file.exists("mapdata/gadm36_HKG_1.shp")){
  
  map_dl <- tempfile()

  # Download the file from my github
  download.file("https://github.com/dickytao/hkproperty/raw/main/gadm36_HKG_shp.zip", map_dl)
  
  # Put the data in mapdata directory
  unzip(map_dl, exdir= "mapdata")
  
} 

# Import Hong Kong map boundary from the shapefile for visualization use
HK_boundary <- st_read("mapdata/gadm36_HKG_1.shp")
HK_boundary %>% 
  st_geometry() %>% 
  class  




## ----Property Data Download , echo=FALSE------------------------------------------------------------------------



#######################################################################
##     Download the HK property data from the github repositories    ##
#######################################################################

if(!file.exists("properties.csv")){
  
  dl <- tempfile()
  download.file("https://github.com/dickytao/hkproperty/raw/main/archive.zip", dl)
  
  # Read data into data frame
  hkproperty <- read.csv(unzip(dl, "properties.csv"), header = FALSE, sep = ",", quote = "\"")

} else {
  
  # This is used for reading data stored locally 
  hkproperty <- read.csv("properties.csv", header = FALSE, sep = ",", quote = "\"")
}
  
# Add the title name for each column
colnames(hkproperty) <- c("type", "location", "estate", "district", "long", "lat", "floor", "unit", "netArea", "grossArea", "totalPrice", "netfeetPrice", "grossfeetPrice", "salesDate", "priceChange", "lastDate" )






## ----Dataset Information, echo=FALSE, tidy=TRUE-----------------------------------------------------------------
# Column Names and first few rows of the dataset
hkproperty %>%
  names() 

# Show the top 10 HK property info in dataset
hkproperty  %>%
  head(10) %>%
  kable() %>%
  kable_styling(latex_options = c("striped", "scale_down", "hold_position"))



## ----Data Wrangling - data cleaning , echo=FALSE----------------------------------------------------------------

# Pick the key and useful factor for price model development
hkproperty <- hkproperty %>%
 subset(select=c("netfeetPrice", "location", "estate", "district", "long", "lat", "floor","netArea", "salesDate"))

# Convert character into number for floor, netArea and netfeetPrice coloumn 
hkproperty[c("netfeetPrice", "long", "lat", "floor", "netArea")] <- lapply(hkproperty[c("netfeetPrice", "long", "lat", "floor", "netArea")], function(x){as.numeric(gsub(",", "", x))})

# Check and count the number of NA 
total_na <- sum(is.na(hkproperty))

# Drop the row of data with either NA or no value in an entry
hkproperty <- hkproperty %>% drop_na()

# Count and remove duplicated entries
duplicated_entry <- sum(duplicated(hkproperty))
hkproperty <- unique(hkproperty)

# Convert transaction date from a character string to date format in R
hkproperty$salesDate <- as.Date(hkproperty$salesDate, "%Y-%m-%d")

# Extract transaction year from the transaction date 
salesYear <- sapply(hkproperty$salesDate, function(x){as.numeric(format(x,'%Y'))})

# Add sales year back into dataset
hkproperty <- hkproperty %>% mutate(salesYear = salesYear)
rm(salesYear)

# Remove Detailed Transaction Date
hkproperty <- hkproperty %>%
  select(-c("salesDate"))



## ----Data Wrangling - data selection , echo=FALSE---------------------------------------------------------------

# Remove those entries which have abnormally low and high per square feet price. According to reference [3], the average price in 2003 (the lowest after 1997)  is HKD2,000. As we are going to analyze pricing data after 2009, we remove those pricing values with less than HK2,000. According to [4], the highest per square feet price in Hong Kong history is HKD137,872.  We will also remove those with per square feet price larger than this number
hkproperty <- hkproperty %>% filter(netfeetPrice < 137872 & netfeetPrice >= 2000)

# Focus our work on the property sales data from 2009 to 2018 (5 Years)
hkproperty <- hkproperty %>% filter(salesYear >= 2009 & salesYear <= 2018)

# Only handle data in Kowloon downtown area 
# We filter data using district. There are 5 districts in downtown area with HASC 
# KC" or "HK.KC", "KU" or "HK.KU", "SS" or "HK.SS", "WT" or "HK.WT", "YT" or "HK.YT"
# However, the district codes in the dataset are not fully the same as HASC with 
# the mapping of the different one in the followings:
# KC=KC, KT=KU, SSP=SS, WTS=WT, YTM=YT
hkproperty <- hkproperty %>%
  filter(district %in% c("KC", "KT", "SSP", "WTS", "YTM")) 

# Convert location, estate, district variables to factors so they can be used as an input to the Random Forest model
hkproperty[c("location", "estate", "district")] <- 
  lapply(hkproperty[c("location", "estate", "district")], function(x){as.factor(x)})




## ----view and remove estate factor, echo=FALSE, tidy=TRUE-------------------------------------------------------

# View the data structure
str(hkproperty)

# Obtain the total number of distinct estate type
total_estate <- as.numeric(n_distinct(hkproperty$estate))

# remove the estate
hkproperty <- hkproperty %>% select(-c("estate"))



## ----Latitude and Longitude , echo=FALSE------------------------------------------------------------------------

# Read lat long from HK property data
property_sf <- st_as_sf(hkproperty, coords = c("long", "lat"), crs=4326)


# Show the locational distribution of the property transactions in different districts in Hong Kong
ggplot(property_sf) +
  geom_sf(data = HK_boundary, aes(fill = NAME_1)) +
  geom_sf(aes(col=netfeetPrice)) +
  guides(fill = FALSE) +
   theme(legend.justification=c(1,0), legend.position=c(1,0),
        legend.title = element_text(size = 8), 
        legend.text = element_text(size = 8))




## ----Plot the house price in selected district, echo=FALSE------------------------------------------------------

# Filter the target districts with the mapping of district code in dataset with 
# the HASC code. The followings are the mappings
# KC=HK.KC, KT=HK.KU, SSP=HK.SS, WTS=HK.WT, YTM=HK.YT
selected_district <- HK_boundary %>%
  filter(HASC_1 %in% c("HK.KC", "HK.KU", "HK.SS", "HK.WT", "HK.YT", "HK.KI"))

# Plot the zoom in areas of the selected district and show the transactions in
# the based map with scaled color gradient to show the price difference. 
ggplot(property_sf) +
  geom_sf(data = selected_district, aes(fill = HASC_1)) +
  scale_colour_gradient(low = "yellow", high = "red", na.value = NA) +
  geom_sf(aes(col=log10(netfeetPrice))) +
  guides(fill = FALSE)  



## ----Plot house price in 2017, echo=FALSE-----------------------------------------------------------------------

# extract 2017 property transaction data out
year2017_sf <- property_sf %>% filter(salesYear == 2017)

# Display the map plot with 2017 transaction data points
ggplot(year2017_sf) +
  geom_sf(data = selected_district, aes(fill = HASC_1)) +
  scale_colour_gradient(low = "white", high = "red", na.value = NA) +
  geom_sf(aes(col=log10(netfeetPrice))) +
  guides(fill = FALSE)  

# remove the memory of 2017 map related data
rm(year2017_sf)

# Remove LOCATION INFO in data training and prediction
hkproperty <- hkproperty %>% select(-c("long", "lat"))




## ----Plot correlation table for numerical varaibles , echo=FALSE------------------------------------------------

corrplot(cor(hkproperty[,sapply(hkproperty, is.numeric)]), method="circle")



## ----histogram of price, echo=FALSE-----------------------------------------------------------------------------

# Property per square feet price histogram
hkproperty %>% 
  ggplot(aes(netfeetPrice)) + 
  geom_histogram(bins = 60, fill = 'dodgerblue4', color = 'white') +
  labs(x = "Per net square feet price (HKD) ", y = "Total Counts") +
  geom_vline(aes(xintercept = mean(netfeetPrice)), color = 'red', size = 1) +
  scale_x_log10(n.breaks = 11, labels = comma) +
  theme(axis.text.x = element_text(hjust = 0.5, size=rel(0.9)))



## ----list of unique location, echo=FALSE------------------------------------------------------------------------

# To show the list of unique location factors
unique(hkproperty$location)




## ----location data statistics , echo=FALSE----------------------------------------------------------------------

# Obtain the total number of property transaction in each location
location_sum <- hkproperty %>%
  group_by(location) %>%
  summarise(Count = n())

# Plot the total count vs location with descending number of counts
location_sum %>%
  ggplot(aes(x = reorder(location, -Count), y = Count)) +
  geom_point(color = 'dodgerblue4') +
  scale_y_log10(n.breaks = 10, labels = comma) +
  geom_hline(aes(yintercept = mean(Count)), color = 'red', size = 0.5) +
  labs(x = "Location", y = "Total Counts") +
  theme(axis.text.x=element_blank())




## ----location head and tail, echo=FALSE-------------------------------------------------------------------------

# Show the top 10 location with the most transactions
location_sum %>%
  arrange(desc(Count)) %>%
  head(10) %>%
  kable( caption = "The Top 10 Locations with the most transaction") %>%
  kable_styling(latex_options = "hold_position")

# Show the top 10 location with the most transactions
location_sum %>%
  arrange(desc(Count)) %>%
  tail(10) %>%
  kable( caption = "The Bottom 10 Locations with the least transaction") %>%
  kable_styling(latex_options = "hold_position")


## ----transaction statistics in district , echo=FALSE, tidy=TRUE-------------------------------------------------

## The total number of property transaction in each district
hkproperty %>%
  ggplot(aes(x=district), data.frame(district)) +
  geom_bar(fill = 'dodgerblue4', color = 'white') +
  scale_y_continuous(n.breaks = 10, labels = comma) +
  labs(x = "District Code", y = "Total Counts") 



## ----transaction price in district, echo=FALSE------------------------------------------------------------------

# Box plot of district vs net per square feet price
hkproperty %>%
  ggplot(aes(x = as.factor(district), y = netfeetPrice,  color = district)) +
  scale_y_log10(n.breaks = 9, labels = comma) +
  geom_boxplot() +
  stat_summary(fun = "mean", geom="point", color = "dodgerblue4", size = 2) +
  labs(x = "District Code", y = "Per net square feet price") +
  theme(axis.text.x = element_text(hjust = 0.5, size=rel(1)), legend.position="none")





## ----proprety floor statistics in district, echo=FALSE----------------------------------------------------------

# Box plot of district vs net per square feet price
hkproperty %>%
  ggplot(aes(x = as.factor(district), y = floor,  color = district)) +
  scale_y_continuous(n.breaks = 9, labels = comma) +
  geom_boxplot() +
  stat_summary(fun = "mean", geom="point", color = "dodgerblue4", size = 2) +
  labs(x = "District Code", y = "Property floor") +
  theme(axis.text.x = element_text(hjust = 0.5, size=rel(1)), legend.position="none")





## ----histogram of property floor, echo=FALSE--------------------------------------------------------------------

# Property per square feet price histogram
hkproperty %>% 
  ggplot(aes(floor)) + 
  geom_histogram(bins = 80, fill = 'dodgerblue4', color = 'white') +
  labs(x = "Property floor", y = "Total Counts") +
  geom_vline(aes(xintercept = mean(floor)), color = 'red', size = 1) +
  scale_x_continuous(n.breaks = 20) +
  theme(axis.text.x = element_text(hjust = 0.5, size=rel(0.9)))



## ----price vs floor, echo=FALSE---------------------------------------------------------------------------------

# box plot of per net square feet price against property floor
hkproperty %>%
  ggplot(aes(x= floor, group = floor, y = netfeetPrice, color = floor)) +
  scale_y_log10(n.breaks = 9, labels = comma) +
  scale_x_continuous(n.breaks = 10) +
  geom_boxplot() +
  stat_summary(fun = "mean", geom="point", color = "red", size = 1) +
  theme(axis.text.x = element_text(hjust = 0.5, size=rel(1)), legend.position="none")



## ----area size histogram, echo=FALSE----------------------------------------------------------------------------

# histogram about the area size of property in HK
hkproperty %>% 
  ggplot(aes(netArea)) + 
  geom_histogram(bins = 60, fill = 'dodgerblue4', color = 'white') +
  geom_vline(aes(xintercept = mean(netArea)), color = 'red', size = 1) +
  labs(x = "Net Area Size in Square Feet ", y = "Total Counts") +
  scale_x_log10(n.breaks = 11) +
  theme(axis.text.x = element_text(hjust = 0.5, size=rel(0.9)))



## ----area size against price, echo=FALSE------------------------------------------------------------------------

# Plot of net per square feet price against net area size of a property
hkproperty %>% 
  ggplot(aes(x= netArea, y = netfeetPrice, color = salesYear)) +
  geom_point(color = 'dodgerblue4') +
  geom_smooth(color = 'red') +
  labs(x = "Net Area Size in Square Feet ", y = "Per net square feet Price (HKD)") +
  theme(axis.text.x = element_text(hjust = 0.5))
  


## ----transaction per year data, echo=FALSE----------------------------------------------------------------------

# get total count of transaction per year
year_sum <- hkproperty %>%
  group_by(salesYear) %>%
  summarise(Count = n())



## ----transcation per year plot, echo=FALSE----------------------------------------------------------------------


# property sales transaction in each year from 2009 to 2018 in HK
hkproperty %>%
  ggplot(aes(x=salesYear), data.frame(salesYear)) +
  geom_bar( fill = "dodgerblue4", color='white') +
  geom_hline(aes(yintercept = length(salesYear)/n_distinct(salesYear)), color = 'red', size = 1) +
  labs(x = "Year", y = "Total Transaction Count") +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 6, labels = comma) +
  theme(axis.text.x = element_text(hjust = 0.5))



## ----price vs sale year, echo=FALSE-----------------------------------------------------------------------------

# box plot of per net square feet price against year
hkproperty %>%
  ggplot(aes(x= salesYear, group = salesYear, y = netfeetPrice, color = salesYear)) +
  scale_y_log10(n.breaks = 9, labels = comma) +
  scale_x_continuous(n.breaks = 10) +
  geom_boxplot() +
  stat_summary(fun = "mean", geom="point", color = "red") +
  theme(axis.text.x = element_text(hjust = 0.5, size=rel(1)), legend.position="none")



## ----Prepare training and testing datasets, echo=FALSE----------------------------------------------------------


# Set sample seed to 8 for replicability of results
#set.seed(8, sample.kind="Rounding")

# We sample 10,000 entries from the data set to have a more manageable size on which to run our algorithms
#hkproperty <- hkproperty %>% sample_n(2000)


#Set sample seed to 1 for replicability of results
set.seed(1, sample.kind="Rounding") 

# Test set will be 20% of HK property's data
test_index <- createDataPartition(y = hkproperty$netfeetPrice, times = 1, p = 0.2, list = FALSE)

# Create training and test datasets
train_set <- hkproperty[-test_index,]
test_set <- hkproperty[test_index,]

# remove unused data memory
if(exists("dl")){

    rm(dl, test_index, removed)
  
  } else {
    
    rm(test_index, removed)
    
  }
  



## ----Mean net sqaure feet price, echo=FALSE---------------------------------------------------------------------

# The RMSE of the predicted result using the mean net square feet price of the training dataset
rmse_simple <- sqrt(mean(( test_set$netfeetPrice - mean(train_set$netfeetPrice) )^2))

# The mean absolute error (MAE) of the predicted result using the mean net square feet price of the training dataset 
mae_simple <- mean(abs(test_set$netfeetPrice - mean(train_set$netfeetPrice)))

# Display the performance and training speed result of simple mean model 
# As we do not need to train the simple mean model, the training duration is 0
train_summary <- tibble(Method = "Simple Average", 
                        Model_RMSE = rmse_simple, 
                        Model_MAE = mae_simple, 
                        Model_Rsquare = NA,
                        Train_Time = as.character.Date(0))

kable(train_summary) %>%
  kable_styling(latex_options = "hold_position")



## ----GLM Model, echo=FALSE--------------------------------------------------------------------------------------

# Put down the start time of training
start.time <- Sys.time()

# Before training K-NN classifier, set.seed().
set.seed(113)

# Develop a prediction model using generalized linear model with
# 5-fold cross validation and repeat 2 times
glm_fit <- train(netfeetPrice ~ ., 
                trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3),
                data=train_set, 
                method = 'glm')

# Put down the training end time
time.taken <- Sys.time() - start.time

# create an empty data frame to store prediction time of all models
predict_time <- data.frame(matrix(ncol = 1, nrow = 4))

# Put down the start time of prediction
start.time <- Sys.time()

# Predict the per square feet price with the model
y_hat_glm <- predict(glm_fit, newdata = test_set)

# calculate and store prediction end time
predict_time[1,] <- as.character.Date(Sys.time() - start.time)

# Display the performance and training speed result of GLM  
train_summary <- bind_rows(train_summary, 
                           tibble(Method = "GLM", 
                           Model_RMSE = glm_fit$results$RMSE , 
                           Model_MAE = glm_fit$results$MAE , 
                           Model_Rsquare = glm_fit$results$Rsquared,
                           Train_Time = as.character.Date(time.taken)))
                          


## ----K-NN Model, echo=FALSE-------------------------------------------------------------------------------------

# Put down the start time of training
start.time <- Sys.time()

# Before training K-NN classifier, set.seed().
set.seed(123)

# Develop a prediction model using K-NN. Set repeated cross validation.
# split data into 5 partitions and repeat 2 times for cross validation
knn_fit <- train(netfeetPrice ~ ., 
                data = train_set, 
                method = "knn",
                tuneGrid = data.frame(k = seq(1, 8, 1)),
                trControl = trainControl(method = "repeatedcv", 
                                         number = 5,
                                         repeats = 3))

# Put down the end time of training
time.taken <- Sys.time() - start.time

# Put down the start time of prediction
start.time <- Sys.time()

# Predict the per square feet price with the model
y_hat_knn <- predict(knn_fit, newdata = test_set)

# calculate and store prediction end time
predict_time[2,] <- as.character.Date(Sys.time() - start.time)

# Display the performance and training speed result of K-NN Model 
train_summary <- bind_rows(train_summary, 
                           tibble(Method = "KNN", 
                           Model_RMSE = getTrainPerf(knn_fit)$TrainRMSE, 
                           Model_MAE = getTrainPerf(knn_fit)$TrainMAE, 
                           Model_Rsquare = getTrainPerf(knn_fit)$TrainRsquared,
                           Train_Time = as.character.Date(time.taken)))
                  



## ----Random Fortress Prediction Model, echo=FALSE, results=FALSE, message=FALSE, warning=FALSE------------------


# Put down the start time of training
start.time <- Sys.time()

# Before training, set.seed().
set.seed(51)

# Develop a prediction model using Random Fortress with 5-fold cross validation
# the process do not repeat as it takes long time
rf_fit <- train(netfeetPrice ~ ., 
                tuneLength = 10,
                trControl = trainControl(method = "cv", number = 5, repeats = 3),
                data = train_set, 
                method = "ranger")  

# Put down the end time of training
time.taken <- Sys.time() - start.time

# Put down the start time of prediction
start.time <- Sys.time()

# Predict the per square feet price with the model
y_hat_rf <- predict(rf_fit, newdata = test_set)

# calculate and store prediction end time
predict_time[3,] <- as.character.Date(Sys.time() - start.time)

# Display the performance and training speed result of Random Forest Model 
train_summary <- bind_rows(train_summary, 
                           tibble(Method = "Random Forest", 
                           Model_RMSE = getTrainPerf(rf_fit)$TrainRMSE, 
                           Model_MAE = getTrainPerf(rf_fit)$TrainMAE, 
                           Model_Rsquare = getTrainPerf(rf_fit)$TrainRsquared,
                           Train_Time = as.character.Date(time.taken)))
                          


## ----Extreme Gradient Boosting, echo=FALSE----------------------------------------------------------------------


# Put down the start time of training
start.time <- Sys.time()

# Before training, set.seed().
set.seed(67)

# Develop a prediction model using Extreme Gradient Boosting
xgb_fit <- train(netfeetPrice ~ ., 
                 trControl = trainControl(method = "repeatedcv", 
                                         number = 5,
                                         repeats = 3),                 
                 data = train_set, 
                 method = "xgbLinear") 

# Put down the training end time
time.taken <- Sys.time() - start.time

# record prediction start time
start.time <- Sys.time()

# Predict the per square feet price with the model
y_hat_xgb <- predict(xgb_fit, newdata = test_set)

# calculate and store prediction end time
predict_time[4,] <- as.character.Date(Sys.time() - start.time)

# Display the performance and training speed result of Extreme Gradient Boosting Model 
train_summary <- bind_rows(train_summary, 
                           tibble(Method = "XGBoost", 
                           Model_RMSE = getTrainPerf(xgb_fit)$TrainRMSE, 
                           Model_MAE = getTrainPerf(xgb_fit)$TrainMAE, 
                           Model_Rsquare = getTrainPerf(xgb_fit)$TrainRsquared,
                           Train_Time = as.character.Date(time.taken)))



## ----K-NN k value plot, echo=FALSE------------------------------------------------------------------------------

# plot the k value to show the local minimum
ggplot(knn_fit, highlight = TRUE) +
  geom_line(color = "dodgerblue4", size = 1) +
  labs(x = "k (Number of neighbors) ", y = "RMSE (Repeated cross validation)") +
  theme(axis.text.x = element_text(hjust = 0.5))



## ----RF mtry value plot, echo=FALSE-----------------------------------------------------------------------------

# plot the k value to show the local minimum
ggplot(rf_fit, highlight = TRUE) +
  labs(x = "mtry (Randomly sampled predictors) ", y = "RMSE (Repeated cross validation)") +
  theme(legend.position = c(0.8, 0.8), 
        legend.background = element_rect(fill = "white", color = "black"))



## ----Model training result, echo=FALSE--------------------------------------------------------------------------

# Show the final results of the 4 regression models with simple average as reference case
kable(train_summary) %>%
  kable_styling(latex_options = "hold_position") 




## ----Result Comparion, echo=FALSE-------------------------------------------------------------------------------


# calculate the RMSE of the prediction results of the 4 models with test set data
test_rmse <- c(sqrt(mean((test_set$netfeetPrice - y_hat_glm)^2)),
               sqrt(mean((test_set$netfeetPrice - y_hat_knn)^2)),
               sqrt(mean((test_set$netfeetPrice - y_hat_rf)^2)),
               sqrt(mean((test_set$netfeetPrice - y_hat_xgb)^2)))

# calculate the MAE of the prediction results of the 4 models with test set data
test_mae <- c(mean(abs(test_set$netfeetPrice - y_hat_glm)),
              mean(abs(test_set$netfeetPrice - y_hat_knn)),
              mean(abs(test_set$netfeetPrice - y_hat_rf)),
              mean(abs(test_set$netfeetPrice - y_hat_xgb)))

# calculate the R-square of the prediction results of the 4 models with test set data
test_Rsquare <- c(cor(test_set$netfeetPrice,y_hat_glm)^2,
                  cor(test_set$netfeetPrice,y_hat_knn)^2,
                  cor(test_set$netfeetPrice,y_hat_rf)^2,
                  cor(test_set$netfeetPrice,y_hat_glm)^2)

# Build the table of to show the summary of model performance based on test set data 
test_summary <- tibble(Method = c("GLM", "K-NN", "RF", "XBGBoost"), 
                       TEST_RMSE = test_rmse, 
                       TEST_MAE = test_mae, 
                       TEST_Rsquare = test_Rsquare,
                       PRED_Time = as.data.frame(predict_time))

kable(test_summary) %>%
  kable_styling(latex_options = "hold_position")




## ----predicted vs acutal result plots, echo=FALSE---------------------------------------------------------------

# GLM model's prediction result vs Actual net square feet price plot
glm_plot <- ggplot() +
  geom_point(aes(x=test_set$netfeetPrice, y=y_hat_glm), color = 'dodgerblue4', size = 0.75) +
  geom_abline(aes(intercept = 0, slope = 1), color = 'red') +
  scale_x_continuous(n.breaks = 3, labels = comma, limits = c(0, 100000)) +
  scale_y_continuous(n.breaks = 3, labels = comma, limits = c(0, 100000)) +
  coord_fixed(ratio = 1) +
  labs(title = "GLM", x = "Actual Square Feet Price", y = "Predicted Square Feet Price") +
  theme(axis.title  = element_text(size = 9), 
        axis.text = element_text(size = 8), 
        plot.title = element_text(size = 10)) 

# K-NN model's prediction result vs Actual net square feet price plot
knn_plot <- ggplot() +
  geom_point(aes(x=test_set$netfeetPrice, y=y_hat_knn), color = 'salmon4', size = 0.75) +
  geom_abline(aes(intercept = 0, slope = 1), color = 'red') +
  scale_x_continuous(n.breaks = 3, labels = comma, limits = c(0, 100000)) +
  scale_y_continuous(n.breaks = 3, labels = comma, limits = c(0, 100000)) +
  coord_fixed(ratio = 1) +
  labs(title = "K-NN", x = "Actual Square Feet Price", y = "Predicted Square Feet Price") +
  theme(axis.title  = element_text(size = 9), 
        axis.text = element_text(size = 8), 
        plot.title = element_text(size = 10)) 
  
# Random Forest model's prediction result vs Actual net square feet price plot
rf_plot <- ggplot() +
  geom_point(aes(x=test_set$netfeetPrice, y=y_hat_rf), color = 'purple4', size = 0.75) +
  geom_abline(aes(intercept = 0, slope = 1), color = 'red') +
  scale_x_continuous(n.breaks = 3, labels = comma, limits = c(0, 100000)) +
  scale_y_continuous(n.breaks = 3, labels = comma, limits = c(0, 100000)) +
  coord_fixed(ratio = 1) +
  labs(title = "RF", x = "Actual Square Feet Price", y = "Predicted Square Feet Price") +
  theme(axis.title  = element_text(size = 9), 
        axis.text = element_text(size = 8), 
        plot.title = element_text(size = 10)) 

# XGBoost model's prediction result vs Actual net square feet price plot
xgb_plot <- ggplot() +
  geom_point(aes(x=test_set$netfeetPrice, y=y_hat_xgb), color = 'seagreen', size = 0.75) +
  geom_abline(aes(intercept = 0, slope = 1), color = 'red') +
  scale_x_continuous(n.breaks = 3, labels = comma, limits = c(0, 100000)) +
  scale_y_continuous(n.breaks = 3, labels = comma, limits = c(0, 100000)) +
  coord_fixed(ratio = 1) +
  labs(title = "XGBoost", x = "Actual Square Feet Price", y = "Predicted Square Feet Price") +
  theme(axis.title  = element_text(size = 9), 
        axis.text = element_text(size = 8), 
        plot.title = element_text(size = 10)) 

# Draw the 2 x 2 plots of the actual versus prediction results for the 4 prediction models
plot_grid(glm_plot, knn_plot, rf_plot, xgb_plot, ncol = 2,  align = "v")


