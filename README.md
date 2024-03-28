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

<p align="right"> 
  <a href="https://www.ericklinenberg.com/books#palaces-for-the-people-how-social-infrastructure-can-help-fight-inequality-polarization-and-the-decline-of-civic-life">
    <img width="100" src="https://github.com/itstrieu/California_Public_Libraries/assets/38932563/57b71c17-686d-4d6a-bbaa-ce583d87f97b">
  </a>
</p>

## Data Description

My analysis focused on California libraries, categorized by community size. Leveraging publicly available data, I scrutinized metrics like library visits, computer usage, and program attendance.

<p align="center"> 
  <img width="420" height="300" src="https://github.com/itstrieu/California_Public_Libraries/assets/38932563/0efae744-0cc1-451b-811a-8ea8f19d3b14">
  <img width="420" height="300" src="https://github.com/itstrieu/California_Public_Libraries/assets/38932563/7415df9f-5cec-4e86-8b74-ae172d6fb4d6">
</p>

## Descriptive Analytics

From 2016 to 2020, library visits dipped, rebounding post-pandemic but falling short of pre-crisis levels. Despite size differences, average visits per person hovered around 3 yearly. Small libraries showed greater resilience in program attendance.

## Model Building

Employing Support Vector Machine Regression (SVMR) and Random Forest models, I probed predictors of library visits. Both models spotlighted the importance of computer usage, non-English materials circulation, and adult programs.

<p align="center">
  <img width="500" height="375" src="https://github.com/itstrieu/California_Public_Libraries/assets/38932563/c5e3a4f0-899f-466c-932b-19a6b7fa749a">
  <img width="500" height="375" src="https://github.com/itstrieu/California_Public_Libraries/assets/38932563/7c65bb8c-c662-4eaa-a7c4-52eb0f5227fa">
</p>

## Model Analysis

While SVMR excelled in prediction accuracy, Random Forest captured more visitation variation. Insights gleaned include adjusting performance metrics and resource allocation strategies.

<p align="center"> 
  <img width="500" height="375" src="https://github.com/itstrieu/California_Public_Libraries/assets/38932563/0ca06871-b19e-4cd1-ba83-57d31143f5c6">
  <img width="500" height="375" src="https://github.com/itstrieu/California_Public_Libraries/assets/38932563/2e388b7c-f466-4537-b1a9-97a4d8b66028">
</p>

## Conclusion

This research arms librarians with tools to predict and grasp library visitation dynamics. Key findings suggest prioritizing experimentation with computer usage, non-English materials circulation, and adult programming. Integrating demographic data in future research is advised for a deeper understanding.
