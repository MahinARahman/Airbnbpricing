#Loading Packages for data cleaning 
install.packages("tidyverse")
install.packages("lubridate")
install.packages("geosphere")
install.packages("partykit")
install.packages("randomForest")
install.packages("naniar")
install.packages("ggplot2")
install.packages("GGally")
install.packages("FNN")

library(tidyverse)
library(lubridate)
library(geosphere)
library(partykit)
library(randomForest)
library(naniar)
library(ggplot2)
library(GGally)
library(glmnet)
library(randomForest)
library(readr)
library(FNN)         
library(rpart)       

#Loading the dataset on airbnb listing in London and checking dimension
airbnb_data <- read_csv("C:/Users/HP/Downloads/listings_london_new.txt")
ncol(airbnb_data)
nrow(airbnb_data)
summary(airbnb_data)

column_types <- sapply(airbnb_data, class)

# Step 2: Create a data frame
type_table <- data.frame(
  Column_Name = names(column_types),
  Data_Type = column_types,
  stringsAsFactors = FALSE
)


#Deleting fields that are either identifiers or URLs or duplicates or ambiguous.
#Detailed rationale provided in project report.

airbnb_data <- airbnb_data %>%
  select(-id, -listing_url, -scrape_id, -picture_url, -last_scraped, -source, -name, -host_name, -host_id, 
         -host_url, -host_thumbnail_url, -host_about, -host_picture_url, -host_neighbourhood, -host_verifications, 
         -host_total_listings_count, -description, - neighborhood_overview, -neighbourhood, 
         -neighbourhood_group_cleansed,-bathrooms_text, -minimum_minimum_nights, 
         -maximum_minimum_nights, -minimum_maximum_nights, -maximum_maximum_nights, 
         -minimum_nights_avg_ntm, -maximum_nights_avg_ntm, -calendar_updated, -has_availability, 
         -availability_365, -calendar_last_scraped, -license, -calculated_host_listings_count, 
         -calculated_host_listings_count_entire_homes, 
         -calculated_host_listings_count_private_rooms, 
         -calculated_host_listings_count_shared_rooms)
ncol(airbnb_data)

# Modifying the fields for better analysis- making numeric, counting words, making factors, etc.

airbnb_data <- airbnb_data %>%
  mutate(
    #Turning neighbourhood, property type, room type and host response time to factors
    neighbourhood_cleansed = factor(neighbourhood_cleansed),
    property_type = factor(property_type),
    room_type = factor(room_type),
    #Replacing empty values with 0
    bathrooms = ifelse(is.na(bathrooms), 0, bathrooms),
    bedrooms = ifelse(is.na(bedrooms), 0, bedrooms),
    beds = ifelse(is.na(beds), 0, beds),
    #Turning host since to years before today to transform into numeric variable
    host_since = as.numeric(difftime(Sys.Date(), ymd(host_since), units = "days")) / 365,
    #Determining airbnbs that have review
    review_status = factor(ifelse(is.na(review_scores_rating), 0, 1))
  )


#Counting the number of amenities 
ncol(airbnb_data)
airbnb_data <- airbnb_data %>%
  mutate(
    amenities_list = str_split(amenities, ", "),
    num_amenities = sapply(amenities_list, length)
  )

# Creating dummy variables for the specific amenities
airbnb_data <- airbnb_data %>%
  mutate(amenities = str_remove_all(amenities, "\\[|\\]|\""))  

# Create dummy variables for specific amenities
specific_amenities <- c("Refrigerator", "Kitchen", "Bathtub", "Wifi", "Dedicated workspace")

for (amenity in specific_amenities) {
  airbnb_data[[amenity]] <- ifelse(grepl(amenity, airbnb_data$amenities), 1, 0)
}

#Removing space to ensure consistency in name and prevent any potential errors
airbnb_data <- airbnb_data %>%
  rename(Dedicated_workspace = `Dedicated workspace`)

str(airbnb_data)

nrow(airbnb_data)

#Turning 'N/A's into NA for host acceptance and host response
airbnb_data <- airbnb_data %>%
  mutate(
    host_acceptance_rate = na_if(host_acceptance_rate, "N/A"),
    host_response_rate = na_if(host_response_rate, "N/A"),
    host_response_time = na_if(host_response_time, "N/A")
  )

# Calculating distance to Big Ben 
big_ben_lat <- 51.5007; big_ben_long <- 0.1246
airbnb_data$distance_to_big_ben <- distHaversine(airbnb_data[, c("latitude", "longitude")], c(big_ben_lat, big_ben_long))

# Calculating distance to British Museum 
british_museum_lat <- 51.5154; british_museum_long <- -0.1264
airbnb_data$distance_to_british_museum <- distHaversine(airbnb_data[, c("latitude", "longitude")], c(british_museum_lat, british_museum_long))

# Calculate distance to Buckingham palace
buckingham_palace_lat <- 51.5014; buckingham_palace_long <- -0.1419
airbnb_data$distance_to_buckingham_palace <- distHaversine(airbnb_data[, c("latitude", "longitude")], c(buckingham_palace_lat, buckingham_palace_long))

# Calculate distance to Tower of London
tower_of_london_lat <- 51.50806; tower_of_london_long <- -0.07611
airbnb_data$distance_to_tower_of_london <- distHaversine(airbnb_data[, c("latitude", "longitude")], c(tower_of_london_lat, tower_of_london_long))

#It is important to highlight that property type is just a detailed version of room type and str(airbnb_data) shows that
# it has 98 categories, this will become a hassle when doing random forest which does not accept more than 53 categories. 
#Therefore, due to the likely insignificant contribution to explaining variance and the high number of categories we are 
# getting rid off property type. 

airbnb_data <- airbnb_data %>%
  select(-property_type)

#Removing redundant variables which we used to derive other potentially insightful variables
airbnb_data <- airbnb_data %>%
  select(-amenities, -amenities_list, -latitude, -longitude) 

ncol(airbnb_data)

# Checking if there are any NA/NaN/Inf values in the 'price' column
missing_counts <- colSums(is.na(airbnb_data))

print(missing_counts)

# Visualizing the missing values 
gg_miss_var(airbnb_data, show_pct = TRUE) + 
  labs(title = "Missing Data in Airbnb Dataset",
       x = "Variables",
       y = "Number of Missing Values (in thousand)") + 
  theme_minimal()

#Handling missing values and NAs
airbnb_data <- airbnb_data %>%
  mutate(price = as.numeric(gsub("[\\$,]", "", price))) %>%  # Remove dollar signs and commas
  filter(!is.na(price) & !is.nan(price) & is.finite(price))  # Remove invalid price values
nrow(airbnb_data)

#Deleting columns that have significant missing values
airbnb_data <- airbnb_data %>%
  select(-host_response_rate, -host_response_time, -reviews_per_month, -first_review, -last_review,-host_acceptance_rate, -host_location, -review_scores_rating, -review_scores_accuracy, -review_scores_cleanliness, -review_scores_checkin, -review_scores_communication, -review_scores_location, -review_scores_value) 

colnames(airbnb_data)

# Checking for remaining NAs in the dataset
colSums(is.na(airbnb_data))

# Removing remaining NAs (if any)
airbnb_data <- na.omit(airbnb_data)
nrow(airbnb_data)
ncol(airbnb_data)
View(airbnb_data)
colnames(airbnb_data)
set.seed(123)
training_indices <- sample(1:nrow(airbnb_data), 0.9 * nrow(airbnb_data))

training_data <- airbnb_data[training_indices, ]
testing_data <- airbnb_data[-training_indices, ]

#Checking if the selection of train and test data is balanced
mean(training_data$price)
mean(testing_data$price)

ggpairs(training_data[,c(2,3,5,18,20)])
ggpairs(testing_data[,c(2,3,5,18,20)])

boxplot(training_data$price, main="Price(in pounds) - Train Data")
boxplot(testing_data$price, main="Price(in pounds) - Test Data")

#Given the similar patterns shown in the ggpairs analysis and the close mean values of training and test data we can say that we made a balanced selection.


#Understanding the correlation

corr_matrix <- cor(training_data[, sapply(training_data, is.numeric)])
library(corrplot)
corrplot(corr_matrix, method = "color", type = "full", order = "hclust", tl.col = "black", tl.srt = 45, tl.cex = 0.8)

# PCA
xdata <- airbnb_data %>% select_if(is.numeric)
pca_data <- prcomp(xdata, scale = TRUE)

explained_variance <- pca_data$sdev^2 / sum(pca_data$sdev^2)
cumulative_variance <- cumsum(explained_variance)

par(mar = c(5, 5, 4, 2) + 0.5) # Increase the top margin slightly
barplot(explained_variance[1:10], names.arg = 1:10, col = "pink",
        main = "Variance Explained by Principal Components",
        xlab = "Principal Components", ylab = "Variance Explained",
        cex.names = 0.8, ylim = c(0, 0.16)) # Adjust the y-axis limits

text(1:10, explained_variance[1:10] + 0.008, 
     labels = scales::percent(explained_variance[1:10]), cex = 0.8)

loadings <- pca_data$rotation[, 1:5]

# PC1: Popularity
v1 <- loadings[order(abs(loadings[, 1]), decreasing = TRUE)[1:ncol(xdata)], 1]
loadingfit1 <- lapply(1:ncol(xdata), function(k) (t(v1[1:k]) %*% v1[1:k] - 3/4)^2)
v1[1:which.min(loadingfit1)]  # Significant loadings for PC1

# PC2: Size and Amenities
v2 <- loadings[order(abs(loadings[, 2]), decreasing = TRUE)[1:ncol(xdata)], 2]
loadingfit2 <- lapply(1:ncol(xdata), function(k) (t(v2[1:k]) %*% v2[1:k] - 3/4)^2)
v2[1:which.min(loadingfit2)]  # Significant loadings for PC2

# PC3: Location and Review Activity
v3 <- loadings[order(abs(loadings[, 3]), decreasing = TRUE)[1:ncol(xdata)], 3]
loadingfit3 <- lapply(1:ncol(xdata), function(k) (t(v3[1:k]) %*% v3[1:k] - 3/4)^2)
v3[1:which.min(loadingfit3)]  # Significant loadings for PC3

# PC4: Location and Availability
v4 <- loadings[order(abs(loadings[, 4]), decreasing = TRUE)[1:ncol(xdata)], 4]
loadingfit4 <- lapply(1:ncol(xdata), function(k) (t(v4[1:k]) %*% v4[1:k] - 3/4)^2)
v4[1:which.min(loadingfit4)]  # Significant loadings for PC4

# It suggests that Airbnbs with high availability but located further away from popular attractions might be more desirable to certain guests.

# PC5: Amenities
v5 <- loadings[order(abs(loadings[, 5]), decreasing = TRUE)[1:ncol(xdata)], 5]
loadingfit5 <- lapply(1:ncol(xdata), function(k) (t(v5[1:k]) %*% v5[1:k] - 3/4)^2)
v5[1:which.min(loadingfit5)]  # Significant loadings for PC5

# Comparing different models through K-fold cross-validation

# Prepare feature matrix and target variable
Mx <- model.matrix(price ~ ., data = airbnb_data)[,-1]  # Remove intercept column
My <- airbnb_data$price 

# K-fold cross-validation setup
nfold <- 10
OOS <- data.frame(lasso=rep(NA, nfold), post_lasso=rep(NA, nfold), random_forest=rep(NA, nfold), linear_regression=rep(NA, nfold))

# Generate fold indices
foldid <- rep(1:nfold, each=ceiling(nrow(airbnb_data)/nfold))[sample(1:nrow(airbnb_data))]

# Performance measure function (Mean Absolute Error)
PerformanceMeasure <- function(actual, prediction) {
  mean(abs(prediction - actual))  # Mean Absolute Error
}

# Lasso cross-validation to find optimal lambda
lasso_cv <- cv.glmnet(Mx, My, alpha = 1, family="gaussian")  # Alpha = 1 for Lasso

# Cross-validation loop
for(k in 1:nfold) { 
  # Train on all but fold `k`
  train <- which(foldid != k)
  test <- which(foldid == k)
  
  # Lasso model
  lasso <- glmnet(Mx[train, ], My[train], family="gaussian", lambda=lasso_cv$lambda.min)
  pred.lasso <- predict(lasso, newx=Mx[test, ])
  OOS$lasso[k] <- PerformanceMeasure(actual=My[test], prediction=pred.lasso)
  
  # Post-Lasso model
  features.min <- which(lasso$beta != 0)
  if(length(features.min) > 0) {
    # Ensure correct reference to My[train] and My[test]
    data.min_train <- data.frame(Mx[train, features.min], My = My[train])  # Corrected reference
    m.lr.pl <- lm(My ~ ., data = data.min_train)  # Fit the Post-Lasso model
    data.min_test <- data.frame(Mx[test, features.min])  # Test data
    pred.lr.pl <- predict(m.lr.pl, newdata = data.min_test)  # Predict using Post-Lasso
    OOS$post_lasso[k] <- PerformanceMeasure(actual = My[test], prediction = pred.lr.pl)  # Measure performance
  } else {
    OOS$post_lasso[k] <- NA  # If no features selected by Lasso, assign NA
  }
  
  # Random Forest model
  m.rf <- randomForest(price ~ ., data=airbnb_data[train, ], ntree=1000, mtry=4, nodesize=5)
  pred.rf <- predict(m.rf, newdata=airbnb_data[test, ])
  OOS$random_forest[k] <- PerformanceMeasure(actual=My[test], prediction=pred.rf)
  
  # Linear Regression model
  m.lm <- lm(price ~ ., data=airbnb_data[train, ])
  pred.lm <- predict(m.lm, newdata=airbnb_data[test, ])
  OOS$linear_regression[k] <- PerformanceMeasure(actual=My[test], prediction=pred.lm)
  
  # K-Nearest Neighbors (K-NN) model
  knn_fit <- knn.reg(train = Mx[train, ], test = Mx[test, ], y = My[train], k = 5)  
  OOS$knn[k] <- PerformanceMeasure(actual = My[test], prediction = knn_fit$pred)
  
  # CART (Classification and Regression Trees) model
  m.cart <- rpart(price ~ ., data = airbnb_data[train, ]) 
  pred.cart <- predict(m.cart, newdata = airbnb_data[test, ])
  OOS$cart[k] <- PerformanceMeasure(actual = My[test], prediction = pred.cart)
  
  print(paste("Iteration", k, "of", nfold, "completed"))
}

# Print cross-validation results
print(OOS)

# Summary of performance
OOS_summary <- colMeans(OOS, na.rm=TRUE)  # Include na.rm=TRUE to ignore NA values
print(OOS_summary)

# Barplot of average performance
par(mar=c(7,5,0.5,1) + 0.3)
barplot(OOS_summary, 
        las=2, 
        xpd=FALSE, 
        xlab="", 
        ylim=c(min(OOS_summary) - 1, max(OOS_summary) + 1), 
        ylab="Mean Absolute Error", 
        col="salmon",
        cex.names=0.8)

#We determine the best model from here and use it to find the OOS R2 and MAE for the test data

# Train the Random Forest model on the training dataset
m.rf.final <- randomForest(price ~ ., data = training_data, ntree = 1000, mtry = 4, nodesize = 5)

# Generate and display the Variable Importance Plot
varImpPlot(m.rf.final, 
           main = "Random Forest: Variable Importance Plot", 
           n.var = min(30, ncol(training_data) - 1),  
           col = "black",
           pch = 19)

# Making predictions on the testing dataset
predictions_rf <- predict(m.rf.final, newdata = testing_data)

# Print predictions
print(predictions_rf)

# Calculate Mean Absolute Error
mae_rf <- mean(abs(predictions_rf - testing_data$price))

# Print the results
print(paste("MAE for Random Forest: ", mae_rf))

# Visualize predictions vs actual prices
plot(testing_data$price, predictions_rf, 
     xlab="Actual Prices(in pounds)", ylab="Predicted Prices(in pounds)", 
     main="Random Forest: Actual vs Predicted Prices")
abline(0, 1, col="red")


# Remove outliers using IQR method
Q1 <- quantile(testing_data$price, 0.25, na.rm = TRUE)  # 1st Quartile
Q3 <- quantile(testing_data$price, 0.75, na.rm = TRUE)  # 3rd Quartile
IQR_value <- Q3 - Q1                                 # Interquartile Range (IQR)

lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

test_data_clean <- testing_data[testing_data$price >= lower_bound & 
                                  testing_data$price <= upper_bound, ]

boxplot(test_data_clean$price, main = "Price(in pounds) - Test Data (Cleaned)")

# Making predictions on the testing dataset
predictions_rf <- predict(m.rf.final, newdata = test_data_clean)

# Print predictions
print(predictions_rf)

# Calculate Mean Absolute Error
mae_rf <- mean(abs(predictions_rf - test_data_clean$price))

# Print the results
print(paste("MAE for Random Forest: ", mae_rf))

# Visualize predictions vs actual prices
plot(test_data_clean$price, predictions_rf, 
     xlab = "Actual Prices(in pounds)", ylab = "Predicted Prices(in pounds)", 
     main = "Random Forest: Actual vs Predicted Prices")
abline(0, 1, col = "red")