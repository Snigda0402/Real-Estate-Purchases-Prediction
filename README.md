# Real Estate Purchases Prediction using Linear Regression 

This project involves analyzing real estate purchases data in Brooklyn from 2016 to 2020 using linear regression to explain housing prices. The analysis consists of two parts: Part I evaluates the model's explanatory proportion and predictive accuracy, while Part II estimates price changes from Q3 2020 to Q4 2020. 

## Skills/Tools Used:

1. R Programming Language
2. Data Cleaning
3. Linear Regression

## Project Overview 

**Step 1: Import and Prepare Data**

Five CSV files provide real estate data within Brooklyn. Only purchases of single-family residences and single-unit apartments/condos are considered. Data standardization involves reformatting, type adjustments, and cleaning across years. The resulting consolidated dataset comprises approximately 119,000 rows. Further data filtering narrows the focus to around 19,000 rows.

**Step 2: Exploratory Data Analysis and Feature Engineering**

Utilizing linear regression, the project aims to explain Brooklyn housing prices from 2016-2020. Initial exploratory data analysis examines price distribution and its associations with other variables. Factors and continuous numeric predictors are considered, alongside variable transformations. Pre-modeling and feature engineering involve constructing linear models, adjusting degrees of freedom, assessing adjusted R^2, RMSE, and addressing violations of OLS assumptions.

**Step 3: Model Refinement**

Uncompetitive sales are identified and removed from the sample. Models are evaluated based on their explanatory proportion and predictive accuracy. Feature engineering includes creating new predictors through interaction terms, polynomial terms, and other techniques. The goal is to achieve a model with minimized RMSE, maximized data proportion, and fewer than 40 model degrees of freedom.

