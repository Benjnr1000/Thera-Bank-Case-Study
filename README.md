# Thera Bank Personal Loan Modeling

A complete analytics and machine-learning workflow to help Thera Bank identify liability customers most likely to take a personal loan. This repository contains everything from data ingestion and EDA through customer segmentation and predictive modeling (CART & Random Forest).

---

## 📘 Project Overview

Thera Bank has 5,000 deposit-only customers, of whom only 480 (9.6 %) converted to personal‐loan customers in a prior marketing campaign. Management wants to improve campaign ROI by targeting customers with a higher probability of converting. This project:

1. **Explores** customer demographics, behavior, and product-holding data  
2. **Segments** customers via K-Means clustering to understand major customer types  
3. **Builds** two classification models (CART decision tree and Random Forest)  
4. **Evaluates** and compares model performance (accuracy, recall, AUC, Gini, KS, concordance/discordance)  
5. **Recommends** the best model for future targeted campaigns  

---

## 🗂️ Repository Structure

- ├── data/
- │ └── Thera_Bank_Personal_Loan_Modelling-dataset-1.xlsx # Raw customer dataset
- │
- ├── scripts/
- │ ├── 01_eda.R # Exploratory Data Analysis & cleaning
- │ ├── 02_clustering.R # K-Means customer segmentation
- │ ├── 03_cart_model.R # CART model development & tuning
- │ └── 04_random_forest.R # Random Forest model & evaluation
- │
- ├── reports/
- │ ├── Thera-Bank-Project.Rmd # RMarkdown source
- │ └── Thera-Bank-Project.pdf # Knit HTML/PDF report
- │
- ├── outputs/ # Generated plots, tables, model objects
- │ ├── cluster_profiles.csv
- │ ├── cart_tree.png
- │ └── rf_variable_importance.png
- │
- ├── README.md # This file

---

## 🛠️ Setup & Dependencies

1. **Install R** (≥ 4.0) and RStudio (recommended).  
2. Install required R packages:
   ```r
   install.packages(c(
     "readxl", "dplyr", "ggplot2", "corrplot", "cluster", "factoextra", "NbClust",
     "caTools", "rpart", "rattle", "randomForest", "ROCR", "ineq", "InformationValue",
     "DataExplorer", "mice", "knitr", "rmarkdown"
   ))

