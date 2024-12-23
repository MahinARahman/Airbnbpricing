# Airbnb Pricing Prediction: Regression Model Using Random Forest
## Overview
We developed a regression model to predict prices for Airbnb listings, incorporating key attributes such as location, property space, amenities, and availability.

## Insights
Although Random Forest emerged as the most accurate model based on mean absolute error (MAE) and indicated that price was majorly influenced by distance to landmarks, amenities and neighborhood, it exhibited a concerning level of deviance, with an MAE of 47 pounds against an average listing price of 200 pounds. Consequently, we determined that the model is not suitable for deployment in its current state. We recommend flagging and separating outliers during production and exploring more robust modeling options to improve predictive accuracy.

## Files
- **Airbnb_R_FINAL.R:** R script for data cleaning, feature engineering, and model training.
- **DS project_Team 6 final.pdf:** Report detailing the methodology, analysis, and insights with visualziations. 
- **README.md:** Project documentation.

## Setup
1. Clone the repository: `git clone https://github.com/MahinARahman/Airbnbpricing.git`
2. Install required R packages:
   ```R
   install.packages(c("tidyverse", "lubridate", "randomForest", "geosphere"))

_______________________________________________
