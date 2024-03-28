# Analysis of California Public Libraries

*tidyverse, doParallel, caret, randomForest, pdp, glmnet, shiny*

This repository contains the following... 

## Presentation Files

1. [Project Report](https://github.com/itstrieu/California_Public_Libraries/blob/main/Project%20Report.pdf)
2. [Presentation Slides](https://github.com/itstrieu/California_Public_Libraries/blob/main/Presentation%20Slides.pdf)
3. [Shiny App](https://itstrieu.shinyapps.io/California_Public_Libraries/)
4. [Shiny App Code](https://github.com/itstrieu/California_Public_Libraries/blob/main/CPL_Shiny_App.R)

## Markdown Files

1. [Combine Data Files](https://github.com/itstrieu/California_Public_Libraries/blob/main/00_CPL_Combine_Data_Files.md)
2. [Data Pre-Processing](https://github.com/itstrieu/California_Public_Libraries/blob/main/01_CPL_Data_PreProcessing.md)
3. [Feature Selection and EDA](https://github.com/itstrieu/California_Public_Libraries/blob/main/02_CPL_Feature_Selection.md)
4. [Random Forests Model](https://github.com/itstrieu/California_Public_Libraries/blob/main/03_CPL_Random_Forests.md) 
5. [Support Vector Machine Regression Model](https://github.com/itstrieu/California_Public_Libraries/blob/main/04_Support_Vector_Machine_Regression.md) 

## Background

Libraries are community lifelines, but challenges like declining visits and the COVID-19 pandemic threaten their vitality. Drawing on sociologist Eric Klinenberg's insights, I delved into California libraries' data to uncover trends and solutions.

## Data Description

My analysis focused on California libraries, categorized by community size. Leveraging publicly available data, I scrutinized metrics like library visits, computer usage, and program attendance.

## Descriptive Analytics

From 2016 to 2020, library visits dipped, rebounding post-pandemic but falling short of pre-crisis levels. Despite size differences, average visits per person hovered around 3 yearly. Small libraries showed greater resilience in program attendance.

## Model Building

Employing Support Vector Machine Regression (SVMR) and Random Forest models, I probed predictors of library visits. Both models spotlighted the importance of computer usage, non-English materials circulation, and adult programs.

## Model Analysis

While SVMR excelled in prediction accuracy, Random Forest captured more visitation variation. Insights gleaned include adjusting performance metrics and resource allocation strategies.

## Conclusion

This research arms librarians with tools to predict and grasp library visitation dynamics. Key findings suggest prioritizing experimentation with computer usage, non-English materials circulation, and adult programming. Integrating demographic data in future research is advised for a deeper understanding.
