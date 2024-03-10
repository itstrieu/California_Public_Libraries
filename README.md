# Analysis of California Public Libraries

*tidyverse, doParallel, caret, randomForest, pdp, glmnet*

This repository contains the following... 

Presentation Files: 
1. [Project Report](https://github.com/itstrieu/California_Public_Libraries/blob/main/Project%20Report.pdf)
2. [Presentation Slides](https://github.com/itstrieu/California_Public_Libraries/blob/main/Presentation%20Slides.pdf)
3. [Shiny App](https://itstrieu.shinyapps.io/California_Public_Libraries/)
4. [Shiny App Code](https://github.com/itstrieu/California_Public_Libraries/blob/main/CPL_Shiny_App.R)

Markdown Files:

1. [Combine Data Files](https://github.com/itstrieu/California_Public_Libraries/blob/main/00_CPL_Combine_Data_Files.md)
2. [Data Pre-Processing](https://github.com/itstrieu/California_Public_Libraries/blob/main/01_CPL_Data_PreProcessing.md)
3. [Feature Selection and EDA](https://github.com/itstrieu/California_Public_Libraries/blob/main/02_CPL_Feature_Selection.md)
4. [Random Forests Model](https://github.com/itstrieu/California_Public_Libraries/blob/main/03_CPL_Random_Forests.md) 
5. [Support Vector Machine Regression Model](https://github.com/itstrieu/California_Public_Libraries/blob/main/04_Support_Vector_Machine_Regression.md) 

## General Description

This project utilizes California Public Libraries Statistics to examine the relationships between library services and attendance/program engagement. By employing a multiple regression model, it reveals the impact of resources and services on community engagement, providing actionable insights for enhanced resource allocation and service improvement to benefit local communities.

## Technical Description

The project utilized a combination of Support Vector Machine Regression (SVMR) and Random Forest models to analyze library metrics and predict visitation patterns. The SVMR model constructs hyperplanes to capture data trends, while the Random Forest model aggregates results from decision trees to provide detailed insights. Data cleansing involved removing observations missing supervisor records and imputing missing values. The models addressed challenges such as high skewness in data distribution and erroneous database entries. Metrics such as Mean Squared Error (MSE), Root Mean Squared Error (RMSE), and Mean Absolute Error (MAE) were used to evaluate model performance. While the SVMR model demonstrated superior prediction quality with lower error rates, the Random Forest model captured more variation in library visits. Partial dependence plots were generated to examine relationships between variables, revealing nuanced insights. 

The models aid librarians by identifying key characteristics with significant influence on library visits. However, they do not establish specific correlative or causal relationships. Instead, they provide a starting point for experimentation, empowering librarians to optimize resource allocation and programming based on informed insights. The feature importance plots highlight variables of interest, guiding librarians in exploring adjustments to enhance community engagement and visitation patterns.
