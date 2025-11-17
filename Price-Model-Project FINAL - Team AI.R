library(forecast)
library(readr)
library(tidyverse)
library(caret)
library(dplyr)

#Load Data - Replace with your pathway
AirBnb_Austin <- read_csv("Fall 2025 - S3/Advanced Data Analytics/Final Project/AirBnb - Austin.csv")
AirBnb_Zurich <- read_csv("Fall 2025 - S3/Advanced Data Analytics/Final Project/AirBnb - Zurich.csv")

####### Price Model ######

##Section 1 - Data Cleaning##

# Checking Data Type of dependent variable (price)
class(AirBnb_Austin$price)
class(AirBnb_Zurich$price)

# Converting data type of price variable to numeric for LM model
AirBnb_Austin <- AirBnb_Austin %>% 
  mutate(price = parse_number(price))

AirBnb_Zurich <- AirBnb_Zurich %>% 
  mutate(price = parse_number(price))

class(AirBnb_Austin$price)
class(AirBnb_Zurich$price)

#Checking for NA values for dependent variable and dropping necessary rows

# Checking NA values for price 
sum(is.na(AirBnb_Austin$price))
sum(is.na(AirBnb_Zurich$price))

# Dropping NA price variables 
AirBnb_Austin <- AirBnb_Austin %>% 
  drop_na(price)

AirBnb_Zurich <- AirBnb_Zurich %>% 
  drop_na(price)

# Checking NA values for predictor variables and dropping necessary rows 

# Property Type - NA Check
sum(is.na(AirBnb_Austin$property_type))
sum(is.na(AirBnb_Zurich$property_type))
  #No NA values in either set, no dropping or imputing needed.

# Room Type - NA Check 
sum(is.na(AirBnb_Austin$room_type))
sum(is.na(AirBnb_Zurich$room_type))
  #No NA values in either set, no dropping or imputing needed.

# Accommodates - NA Check
sum(is.na(AirBnb_Austin$accommodates))
sum(is.na(AirBnb_Zurich$accommodates))
  #No NA values in either set, no dropping or imputing needed.

# Bathrooms - NA Check
sum(is.na(AirBnb_Austin$bathrooms))
sum(is.na(AirBnb_Zurich$bathrooms))

# Dropping NA bathroom rows in both dataframes (only 4 in each)
AirBnb_Austin <- AirBnb_Austin %>% 
  drop_na(bathrooms)

AirBnb_Zurich <- AirBnb_Zurich %>% 
  drop_na(bathrooms)

# Bedrooms - NA Check
sum(is.na(AirBnb_Austin$bedrooms))
sum(is.na(AirBnb_Zurich$bedrooms))
# Dropping NA bedroom rows in both dataframes (16 of 10704 in Austin and 6 of 2499 in Zurich)
AirBnb_Austin <- AirBnb_Austin %>% 
  drop_na(bedrooms)
AirBnb_Zurich <- AirBnb_Zurich %>% 
  drop_na(bedrooms)

# Beds - NA Check
sum(is.na(AirBnb_Austin$beds))
sum(is.na(AirBnb_Zurich$beds))
# Dropping NA bedroom rows in both dataframes (14 of 10704 in Austin and 1 of 2499 in Zurich)
AirBnb_Austin <- AirBnb_Austin %>% 
  drop_na(beds)
AirBnb_Zurich <- AirBnb_Zurich %>% 
  drop_na(beds)


# Minimum Nights - NA Check 
sum(is.na(AirBnb_Austin$minimum_nights))
sum(is.na(AirBnb_Zurich$minimum_nights))
  #No remaining NA values in either set, no dropping or imputing needed.

# Maximum Nights - NA Check
sum(is.na(AirBnb_Austin$maximum_nights))
sum(is.na(AirBnb_Zurich$maximum_nights))
  #No remaining NA values in either set, no dropping or imputing needed.

# SuperHost? - NA Check
sum(is.na(AirBnb_Austin$host_is_superhost))
sum(is.na(AirBnb_Zurich$host_is_superhost))
  #408 remaining NA values for Austin and 146 for Zurich
  #While this may be an important variable, we deem this to be too many 
  #NA rows to remove. Due to the nature of this variable, we deem the risk
  #of imputing a True or False to be too risky. No rows will be dropped
  #based in this variable, and it will not be used.

# Profile Picture - NA Check
sum(is.na(AirBnb_Austin$host_has_profile_pic))
sum(is.na(AirBnb_Zurich$host_has_profile_pic))

#Identity verified? - NA Check
sum(is.na(AirBnb_Austin$host_identity_verified))
sum(is.na(AirBnb_Zurich$host_identity_verified))

#Listings Count - NA Check
sum(is.na(AirBnb_Austin$host_listings_count))
sum(is.na(AirBnb_Zurich$host_listings_count))

# Dropping NA values for profile picture
  #this will also drop the NA values for identity_verified and listings_count
AirBnb_Zurich <- AirBnb_Zurich %>% 
  drop_na(host_has_profile_pic)

#Recheck NA values for listings count and identity verified
#Identity verified? - NA Check - post Profilepic drop
sum(is.na(AirBnb_Austin$host_identity_verified))
sum(is.na(AirBnb_Zurich$host_identity_verified))

#Listings Count - NA Check - post profilepic drop
sum(is.na(AirBnb_Austin$host_listings_count))
sum(is.na(AirBnb_Zurich$host_listings_count))

##Section 2 - Initial Data Visualization##
#Price Boxplot - Austin
ggplot(data=AirBnb_Austin) +
  geom_boxplot(aes(x = price)) + 
  labs(title='Austin: Price Boxplot')
#Price Boxplot - Austin - Max Price = $10000
ggplot(data=AirBnb_Austin) +
  geom_boxplot(aes(x = price)) + 
  xlim(0,10000) + 
  labs(title='Austin: Price Boxplot (Max Price: $10000)')
#Price Boxplot - Austin - Max Price = $5000
ggplot(data=AirBnb_Austin) +
  geom_boxplot(aes(x = price)) + 
  xlim(0,5000) + 
  labs(title='Austin: Price Boxplot (Max Price: $5000')
#Price Histogram - Austin - Max Price = $5000
ggplot(data=AirBnb_Austin)+
  geom_histogram(aes(x=price), binwidth=50)+
  xlim(0,1250) +
  labs(title='Austin: Price Histogram')

#The boxplot and histogram both show that there are extreme outliers in the
#Austin dataset. Therefore we recommend removing any listings with a price 
#over $1250
AirBnb_Austin |>
  count(price > 1250)

#Price Boxplot - Zurich
ggplot(data=AirBnb_Zurich) +
  geom_boxplot(aes(x = price)) + 
  labs(title='Zurich: Price Boxplot')
#Price Boxplot - Zurich - Max Price = $1000
ggplot(data=AirBnb_Zurich) +
  geom_boxplot(aes(x = price)) + 
  xlim(0,1000) + 
  labs(title='Zurich: Price Boxplot (Max Price: $1000)')
#Price Histogram - Zurich - Max Price = $875
ggplot(data=AirBnb_Zurich)+
  geom_histogram(aes(x=price), binwidth=50)+
  xlim(0,875) +
  labs(title='Zurich: Price Histogram')

#The boxplot and histogram both show that there are extreme outliers in the
#Zurich dataset. Therefore we recommend removing any listings with a price 
#over $875
AirBnb_Zurich |>
  count(price > 875)

##Drop Outliers##
AirBnb_Austin_Clean <- AirBnb_Austin |>
  filter(price<=1250)
#Zurich
AirBnb_Zurich_Clean <- AirBnb_Zurich |>
  filter(price<=875)

##Section 3 - Preparing Dataframes for Linear Regression##

#Remove variables that will not be used in the regression
Austin_ColstoKeep = names((AirBnb_Austin_Clean[, c(1,23,26,27,34,35,36,38,39,41,42,43)]))
AirBnb_Austin_Clean = AirBnb_Austin_Clean |>
  select(all_of(Austin_ColstoKeep))
#Zurich
Zurich_ColstoKeep = names((AirBnb_Zurich_Clean[, c(1,23,26,27,34,35,36,38,39,41,42,43)]))
AirBnb_Zurich_Clean = AirBnb_Zurich_Clean |>
  select(all_of(Zurich_ColstoKeep))

#Split Host listings into factor buckets
#Austin
AirBnb_Austin_Clean$host_listings_count <- factor(cut(
    AirBnb_Austin_Clean$host_listings_count,
    breaks = c(0, 1, 10, 30, Inf),
    labels = c("1","2-10","10-30","30+"),
    right = TRUE,
    include.lowest = TRUE),
  levels = c("1","2-10","10-30","30+"),
  ordered = TRUE)
#Zurich
AirBnb_Zurich_Clean$host_listings_count <- factor(cut(
  AirBnb_Zurich_Clean$host_listings_count,
  breaks = c(0, 1, 10, 30, Inf),
  labels = c("1","2-10","10-30","30+"),
  right = TRUE,
  include.lowest = TRUE),
levels = c("1","2-10","10-30","30+"),
ordered = TRUE)

#Factor remaining applicable variables
#Austin
AirBnb_Austin_Clean = AirBnb_Austin_Clean |>
  mutate(across(where(is.character), as.factor))
AirBnb_Austin_Clean = AirBnb_Austin_Clean |>
  mutate(across(c(host_has_profile_pic, host_identity_verified), as.factor))
#Zurich
AirBnb_Zurich_Clean = AirBnb_Zurich_Clean |>
  mutate(across(where(is.character), as.factor))
AirBnb_Zurich_Clean = AirBnb_Zurich_Clean |>
  mutate(across(c(host_has_profile_pic, host_identity_verified), as.factor))

#Create Log Price variable
  #Need to do regression with log price due to right skew (see Histograms)
AirBnb_Austin_Clean$LogPrice = log(AirBnb_Austin_Clean$price+1)
AirBnb_Zurich_Clean$LogPrice = log(AirBnb_Zurich_Clean$price+1)

#Check LogPrice is Normally Distributed
#LogPrice Histogram - Austin
ggplot(data=AirBnb_Austin_Clean)+
  geom_histogram(aes(x=LogPrice))+
  labs(title='Austin: LogPrice Histogram')
#LogPrice Histogram - Zurich
ggplot(data=AirBnb_Zurich_Clean)+
  geom_histogram(aes(x=LogPrice))+
  labs(title='Zurich: LogPrice Histogram')

# Split both datasets into training and testing datasets
set.seed(123)

Austin_tot_obs <- dim(AirBnb_Austin_Clean)[1] 
A_train_size <- floor(0.70 * Austin_tot_obs)
A_test_size = Austin_tot_obs-A_train_size
Austin_train <- sample_n(AirBnb_Austin_Clean, A_train_size) 
Austin_test <- anti_join(AirBnb_Austin_Clean, Austin_train)

Zurich_tot_obs <- dim(AirBnb_Zurich_Clean)[1]
Z_train_size <- floor(0.70*Zurich_tot_obs)
Z_test_size = Zurich_tot_obs-Z_train_size
Zurich_train <- sample_n(AirBnb_Zurich_Clean, Z_train_size) 
Zurich_test <- anti_join(AirBnb_Zurich_Clean, Zurich_train)

## Section 4 - Run initial Linear Regression Models##
#Austin
Austin_model_init = lm(LogPrice ~ .- id - price, data=Austin_train)
summary(Austin_model_init)
#Zurich
Zurich_model_init = lm(LogPrice ~ .-id -price, data=Zurich_train)
summary(Zurich_model_init)

##Section 5 - Final Linear Regression Models

#Use step function to improve model
Austin_model_fin = step(Austin_model_init, direction="both")
Zurich_model_fin = step(Zurich_model_init, direction="both")
summary(Austin_model_fin)
summary(Zurich_model_fin)

#check residuals of models
#Austin
Austin_model_residuals = residuals(Austin_model_fin)
#Zurich
Zurich_model_residuals = residuals(Zurich_model_fin)

#plot residuals
#Austin
ggplot() +
  geom_point(aes(Austin_model_fin$fitted.values, Austin_model_residuals))
#Zurich
ggplot() +
  geom_point(aes(Zurich_model_fin$fitted.values, Zurich_model_residuals))
#residual plots normal, upper edge likely due to dependent variable truncation

##Section 6 - Prediction and Accuracy##
#Predictions
#Austin
Austin_predictions_log = predict(Austin_model_fin, newdata=Austin_test, type="response")
Austin_predictions_price = exp(Austin_predictions_log)-1
Austin_test$Price_Prediction = Austin_predictions_price
#Zurich
Zurich_predictions_log = predict(Zurich_model_fin, newdata=Zurich_test, type="response")
Zurich_predictions_price = exp(Zurich_predictions_log)-1
Zurich_test$Price_Prediction = Zurich_predictions_price

#accuracy
#Austin
accuracy(Austin_predictions_price, x=Austin_test$price)
#Zurich
accuracy(Zurich_predictions_price, x=Zurich_test$price)

#plot predictions vs actual
#Austin
ggplot(data=Austin_test)+
  geom_point(aes(x=price, y=Austin_predictions_price, color="Price vs Prediction"))+
  geom_abline(slope = 1, intercept = 0, color = "darkgreen", linetype = "dashed")+
  labs(title="Austin Predicted vs Actual Price", x = "Actual Price", y = "Predicted Price")
#Zurich
ggplot(data=Zurich_test)+
  geom_point(aes(x=price, y=Zurich_predictions_price, color="Price vs Prediction"))+
  geom_abline(slope = 1, intercept = 0, color = "darkgreen", linetype = "dashed")+
  labs(title="Zurich Predicted vs Actual Price", x = "Actual Price", y = "Predicted Price")


#What if we just guessed the median price of the train data?
#Austin
Austin_med = rep(median(Austin_train$price),A_test_size)
accuracy(Austin_med, x=Austin_test$price)
#Zurich
Zurich_med = rep(median(Zurich_train$price),Z_test_size)
accuracy(Zurich_med, x=Zurich_test$price)
  #For both Austin and Zurich, our model does better than just predicting the
  #median price for the given location

#What if we just guessed the mean price of the train data?
#Austin
Austin_mean = rep(mean(Austin_train$price),A_test_size)
accuracy(Austin_mean, x=Austin_test$price)
#Zurich
Zurich_mean = rep(mean(Zurich_train$price),Z_test_size)
accuracy(Zurich_mean, x=Zurich_test$price)
  #For both Austin and Zurich, our model does better than just predicting the
  #mean price for the given location

##Section 7 - Consolidating and Interpreting Results##
#Austin
Austin_coefficients_log = coefficients(Austin_model_fin)
Austin_percent_change = (exp(Austin_coefficients_log)-1)*100
Austin_estimated_dollar_impact = (Austin_percent_change/100)*median(Austin_train$price) #estimating based on median price in training set
Austin_Results = data.frame(Austin_coefficients_log,Austin_percent_change,Austin_estimated_dollar_impact)
Austin_Results = Austin_Results |>
  mutate(Austin_percent_change = round(Austin_percent_change, 2)) |>
  mutate(Austin_estimated_dollar_impact = round(Austin_estimated_dollar_impact, 2))
Austin_Results[1,2:3]="NA"
print(Austin_Results)
#Zurich
Zurich_coefficients_log = coefficients(Zurich_model_fin)
Zurich_percent_change = (exp(Zurich_coefficients_log)-1)*100
Zurich_estimated_dollar_impact = (Zurich_percent_change/100)*median(Zurich_train$price) #estimating based on median price in training set
Zurich_Results = data.frame(Zurich_coefficients_log,Zurich_percent_change,Zurich_estimated_dollar_impact)
Zurich_Results = Zurich_Results |>
  mutate(Zurich_percent_change = round(Zurich_percent_change, 2)) |>
  mutate(Zurich_estimated_dollar_impact = round(Zurich_estimated_dollar_impact, 2))
Zurich_Results[1,2:3]="NA"
print(Zurich_Results)
