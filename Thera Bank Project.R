#======================================================================= 
# 
# Exploratory Data Analysis - CardioGoodFitness 
# 
#=======================================================================

# Environment set up and data import

# Invoking libraries
library(readxl) # To import excel files
library(ggplot2) # To create plots
library(corrplot) # To plot correlation plot between numerical variables
library(dplyr) # To manipulate dataset
library(gridExtra) # To plot multiple ggplot graphs in a grid
library(DataExplorer) # visual exploration of data
library(mice) # Multivariate Imputation via Chained Equations; takes care of uncertainty in missing values
library(cluster)
library(factoextra) # extract and visualize the results of multivariate data analysis
library(NbClust) # to find optimal number of clusters
library(caTools) # Split Data into Test and Train Set
library(rpart) # To build CART decision tree
library(rattle) # To visualise decision tree
library(randomForest) # To build a Random Forest
library(ROCR) # To visualise the performance classifiers
library(ineq) # To calculate Gini
library(InformationValue) # For Concordance-Discordance
library(knitr) # Necessary to generate source codes from a .Rmd File
library(markdown) # To convert to HTML
library(rmarkdown) # To convret analyses into high quality documents

# Set working directory 
setwd("C:/Users/egwuc/Desktop/PGP-DSBA-UT Austin/Machine Learning/Week 5 - Project/")

# Read input file
thera_bank <- read_excel("Thera Bank_Personal_Loan_Modelling-dataset-1.xlsx", sheet = 2)

# Global options settings
options(scipen = 999) # turn off scientific notation like 1e+06

# Check dimension of dataset 
dim(thera_bank)

# Check first 6 rows(observations) of dataset
head(thera_bank)
tail(thera_bank)

# Convert column names to appropriate column names
colnames(thera_bank) <- c("ID", "Age", "Experience", "Income", "ZIP_Code", "Family_members", "CCAvg", "Education", "Mortgage",
                          "Personal_Loan", "Securities_Account", "CD_Account", "Online", "CreditCard")

# Check structure of dataset
str(thera_bank)

# Change personal_loan to factor variable
thera_bank$Personal_Loan <- as.factor(thera_bank$Personal_Loan)

# Get summary of dataset
summary(thera_bank)

# Dropping ID and Zip Code column
thera_bank <- thera_bank[, -1]
thera_bank <- thera_bank[, -4]

# View the dataset 
View(thera_bank)

# Filter out values less than 0 in Experience
filter(thera_bank, Experience < 0)

# Replace values less than 0 in Experience with 0
thera_bank$Experience <- replace(thera_bank$Experience, thera_bank$Experience<0, 0)

# Check if any values in less than 0 in Experience
filter(thera_bank, Experience < 0)

# How many missing vaues do we have?
sum(is.na(thera_bank)) 

# What columns contain missing values?
colSums(is.na(thera_bank))

# Use functions and algorithms to impute the missing values
data1 <- thera_bank
sum(is.na(data1))
md.pattern(data1)
init.impute = mice(data1, m = 5, method = "pmm", seed = 1000)
thera_bank = complete(init.impute, 2)
md.pattern(thera_bank)
sum(is.na(thera_bank))

# Distribution of the dependent variable
prop.table(table(thera_bank$Personal_Loan))

plot_histogram_n_boxplot = function(variable, variableNameString, binw){
  
  a = ggplot(data = thera_bank, aes(x= variable)) +
    labs(x = variableNameString,y ='count')+
    geom_histogram(fill = 'green',col = 'white', binwidth = binw) +
    geom_vline(aes(xintercept = mean(variable)),
               color = "black", linetype = "dashed", size = 0.5)
  
  b = ggplot(data = thera_bank, aes('',variable))+ 
    geom_boxplot(outlier.colour = 'red',col = 'red', outlier.shape = 19)+
    labs(x = '', y = variableNameString) + coord_flip()
  grid.arrange(a,b,ncol = 2)
}

plot_histogram_n_boxplot(thera_bank$Age, 'Age', 2)

plot_histogram_n_boxplot(thera_bank$Experience, 'Experience', 2)

plot_histogram_n_boxplot(thera_bank$Income, 'Income', 10)

plot_histogram_n_boxplot(thera_bank$Family_members, 'Family Members', 1)

plot_histogram_n_boxplot(thera_bank$CCAvg, 'CCAvg', 1)

plot_histogram_n_boxplot(thera_bank$Education, 'Education', 1)

plot_histogram_n_boxplot(thera_bank$Mortgage, 'Mortgage', 100)

plot_histogram_n_boxplot(thera_bank$Securities_Account, 'Securities Account', 1)

plot_histogram_n_boxplot(thera_bank$CD_Account, 'CD Account', 1)

plot_histogram_n_boxplot(thera_bank$Online, 'Online', 1)

plot_histogram_n_boxplot(thera_bank$CreditCard, 'Credit Card', 1)

# Function to draw percent stacked barchart to see the effect of independent variables
# on the probability of personal loan using ggplot
plot_stacked_barchart = function(variable, variableNameString){
  ggplot(thera_bank, aes(fill = Personal_Loan, x = variable)) + 
    geom_bar(position="fill")+
    labs(title = variableNameString, y = '', x = '')+
    scale_fill_manual(values=c("#0073C2FF", "#EFC000FF"))
        
}

plot_stacked_barchart(thera_bank$Age, 'Age')

plot_stacked_barchart(thera_bank$Experience, 'Experience')

plot_stacked_barchart(thera_bank$Income, 'Income')

plot_stacked_barchart(thera_bank$Family_members, 'Family Members')

plot_stacked_barchart(thera_bank$Education, 'Education')

plot_stacked_barchart(thera_bank$Securities_Account, 'Securities Account')

plot_stacked_barchart(thera_bank$CD_Account, 'CD Account')

plot_stacked_barchart(thera_bank$Online, 'Online')

plot_stacked_barchart(thera_bank$CreditCard, 'Credit Card')

# Numeric variables in the data
num_vars = sapply(thera_bank, is.numeric)

# Correlation Plot
corrplot(cor(thera_bank[,num_vars]), method = 'number')

# Scale the dataset to reduce the influence from variables with high values
data <- thera_bank

# Change Family_members, Education, Securities_Account, CD_Account, Online and CreditCard to factor variable
thera_bank$Family_members <- as.factor(thera_bank$Family_members)
thera_bank$Education <- as.factor(thera_bank$Education)
thera_bank$Securities_Account <- as.factor(thera_bank$Securities_Account)
thera_bank$CD_Account <- as.factor(thera_bank$CD_Account)
thera_bank$Online <- as.factor(thera_bank$Online)
thera_bank$CreditCard <- as.factor(thera_bank$CreditCard)

str(data)
view(data)

thera_bank.scaled <- scale(data[, -c(4, 6, 8, 9, 10, 11, 12)])

# Determine the optimum number of clusters (find optimal k)
seed <- 1000
set.seed(seed) # kmeans uses a randomized starting point for cluster centroids
clust2 = kmeans(thera_bank.scaled, centers = 2, nstart = 5)
print(clust2)

# Visualise the cluster
clusplot(thera_bank.scaled, clust2$cluster, 
         color=TRUE, shade=TRUE, labels=2, lines=1)

# Create clusters for k=3, k=4 and k=5 for comparative analysis
clust3 <- kmeans(thera_bank.scaled, centers = 3, nstart = 5)
clust4 <- kmeans(thera_bank.scaled, centers = 4, nstart = 5)
clust5 <- kmeans(thera_bank.scaled, centers = 5, nstart = 5)

# Visualise clusters in 2 dimensions
k_clust_viz_2 = fviz_cluster(list(data = thera_bank.scaled,
                                  cluster = clust2$cluster)) + 
  ggtitle("k = 2")
k_clust_viz_3 = fviz_cluster(list(data = thera_bank.scaled,
                                  cluster = clust3$cluster)) + 
  ggtitle("k = 3")
k_clust_viz_4 = fviz_cluster(list(data = thera_bank.scaled,
                                  cluster = clust4$cluster)) + 
  ggtitle("k = 4")
k_clust_viz_5 = fviz_cluster(list(data = thera_bank.scaled,
                                  cluster = clust5$cluster)) + 
  ggtitle("k = 5")

# Visualise all 4 clustering plots together
grid.arrange(k_clust_viz_2, k_clust_viz_3, k_clust_viz_4, k_clust_viz_5, nrow = 2)

# To find the optimal numbner of clusters. Lets try K = 1 to 5 and for each plot the "sum of Within cluster sum of squares".
totWss <- rep(0,5)
for(k in 1:5){
  set.seed(seed)
  clust <- kmeans(thera_bank.scaled, centers = k, nstart = 5)
  totWss[k] <- clust$tot.withinss
}
print(totWss)
plot(c(1:5), totWss, type="b", xlab="Number of Clusters",
       ylab="sum of 'Within groups sum of squares'")

set.seed(seed) 
nc <- NbClust(thera_bank.scaled, min.nc = 2, max.nc = 5, method="kmeans")

table(nc$Best.n[1,])

# Adding the cluster numbers back to the dataset
data$Clusters = clust3$cluster

# Aggregate all columns except column 8 for each cluster by their means
custProfile = aggregate(data[, -c(4, 6, 8, 9, 10, 11, 12)],list(data$Cluster),FUN="mean")
print(custProfile)

set.seed(seed) # To ensure reproducibility
split <- sample.split(thera_bank$Personal_Loan, SplitRatio = 0.7)
train <- subset(thera_bank, split == TRUE)
test <- subset(thera_bank, split == FALSE)

nrow(train)
nrow(test)

# Check that the distribution of the dependent variable is similar in train and test sets
prop.table(table(thera_bank$Personal_Loan))
prop.table(table(thera_bank$Personal_Loan))
prop.table(table(thera_bank$Personal_Loan))

# Setting the control parameters (to control the growth of the tree)
# Set the control parameters very low to let the tree grow deep

r.ctrl = rpart.control(minsplit = 50, minbucket = 10, cp = 0, xval = 10)

# Building the CART model

# formula - response variable~predictor variables  
# data - dataset
# method - "class" - for classification, "anova" for regression
# control - tree control parameters

model1 <- rpart(formula = Personal_Loan ~ ., data = train, method = "class", control = r.ctrl)
model1

# Displaying the decision tree
fancyRpartPlot(model1)

# The cost complexity table can be obtained using the printcp or plotcp functions
printcp(model1)
plotcp(model1)

model2 = prune(model1, cp= 0.021, "CP")
printcp(model2)
model2

#Displaying the decision tree
fancyRpartPlot(model2)

# Variable importance is generally computed based on the corresponding reduction of predictive accuracy 
# when the predictor of interest is removed.
model1$variable.importance

# Variable importance is generally computed based on the corresponding reduction of predictive accuracy 
# when the predictor of interest is removed.
model2$variable.importance

# Predicting on the train dataset
train_predict.class1 <- predict(model1, train, type="class") # Predicted Classes
train_predict.score1 <- predict(model1, train) # Predicted Probabilities

# Create confusion matrix for train data predictions
tab.train1 = table(train$Personal_Loan, train_predict.class1)
tab.train1

# Accuracy on train data
accuracy.train1 = sum(diag(tab.train1)) / sum(tab.train1)
accuracy.train1

# Predicting on the train dataset
train_predict.class2 <- predict(model2, train, type="class") # Predicted Classes
train_predict.score2 <- predict(model2, train) # Predicted Probabilities

# Create confusion matrix for train data predictions
tab.train2 = table(train$Personal_Loan, train_predict.class2)
tab.train2

# Accuracy on train data
accuracy.train2 = sum(diag(tab.train2)) / sum(tab.train2)
accuracy.train2

# Predicting on the test dataset using MODEL 1
test_predict.class1 <- predict(model1, test, type="class") # Predicted Classes
test_predict.score1 <- predict(model1, test) # Predicted Probabilities

# Create confusion matrix for test data predictions (using MODEL 1)
tab.test1 = table(test$Personal_Loan, test_predict.class1)
tab.test1

# Accuracy on train data (MODEL 1 predictions)
accuracy.test1 = sum(diag(tab.test1)) / sum(tab.test1)
accuracy.test1

# Predicting on the test dataset using MODEL 2
test_predict.class2 <- predict(model2, test, type="class") # Predicted Classes
test_predict.score2 <- predict(model2, test) # Predicted Probabilities

# Create confusion matrix for test data predictions (using MODEL 2)
tab.test2 = table(test$Personal_Loan, test_predict.class2)
tab.test2

# Accuracy on train data (MODEL 2 predictions)
accuracy.test2 = sum(diag(tab.test2)) / sum(tab.test2)
accuracy.test2

Model_Name = c("Baseline", "Model1", "Model2")
Train_Accuracy_perc = c(90, accuracy.train1*100, accuracy.train2*100)
Test_Accuracy_perc = c(90, accuracy.test1*100, accuracy.test2*100)
output = data.frame(Model_Name,Train_Accuracy_perc,Test_Accuracy_perc)
output

set.seed(seed)
# Formula - response variable ~ predictor variables
# To build a classification random forest the response variable should be converted to a factor if it isn't already a factor
# data -  dataset to train the model on
## Random Forest hyperparameters
# ntree - Total number of trees are to be constructed
# mtry - number of variables tried at each split
# importance - Set TRUE to assess variable importance

rf_model1 = randomForest(
  Personal_Loan ~ .,
  data = train,
  ntree = 501,
  mtry = 5,
  nodesize = 10,
  importance = TRUE
  )

print(rf_model1)

plot(rf_model1, main="")
legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates Random Forest Thera_Bank")

importance(rf_model1)

# Check the column number of the response variable
names(train)

set.seed(seed) # To ensure reproducibility

rf_model2 = tuneRF(x = train[, -8], # matrix or data frame of predictor/independent variables
                  y = train$Personal_Loan, # response vector (factor for classification, numeric for regression)
                  mtrystart = 5, # starting value of mtry
                  stepfactor=1.5, # at each iteration, mtry is inflated (or deflated) by this value
                  ntree=51, # number of trees built for each mtry value
                  improve=0.0001, # the (relative) improvement in OOB error must be by this much for the search to continue
                  nodesize=10, # Minimum size of terminal nodes
                  trace=TRUE, # prints the progress of the search
                  plot=TRUE, # to get the plot of the OOB error as function of mtr
                  doBest=TRUE, # return a forest using the optimal mtry found
                  importance=TRUE # 
                  )

# Predicting on the train dataset
train_predict.class_RF <- predict(rf_model2, train, type = "class") # Predicted Classes
train_predict.score_RF <- predict(rf_model2, train, type = 'prob') # Predicted Probabilities

# Create confusion matrix for train data predictions
tab.train_RF = table(train$Personal_Loan, train_predict.class_RF)
tab.train_RF

# Accuracy on train data
accuracy.train_RF = sum(diag(tab.train_RF)) / sum(tab.train_RF)
accuracy.train_RF

# Predicting on the test dataset
test_predict.class_RF <- predict(rf_model2, test, type = "class") # Predicted Classes
test_predict.score_RF <- predict(rf_model2, test, type = 'prob') # Predicted Probabilities

# Create confusion matrix for test data predictions
tab.test_RF = table(test$Personal_Loan, test_predict.class_RF)
tab.test_RF

# Accuracy on test data
accuracy.test_RF = sum(diag(tab.test_RF)) / sum(tab.test_RF)
accuracy.test_RF

Model_Name = c("Baseline", "CART", "Random Forest")
Train_Accuracy_perc = c(90, accuracy.train1*100, accuracy.train_RF*100)
Test_Accuracy_perc = c(90, accuracy.test1*100, accuracy.test_RF*100)
output = data.frame(Model_Name, Train_Accuracy_perc, Test_Accuracy_perc)
output

varImpPlot(rf_model2, sort = TRUE)

# Predict on test data using cart_model1
model1_predict_class = predict(model1, test, type = 'class')
model1_predict_score = predict(model1, test, type = 'prob')

# Predict on test data using cart_model2
model2_predict_class = predict(model2, test, type = 'class')
model2_predict_score = predict(model2, test, type = 'prob')

# Predict on test data using rf_model1
rf_model1_predict_class = predict(rf_model1, test, type = 'class')
rf_model1_predict_score = predict(rf_model1, test, type = 'prob')

# Predict on test data using rf_model2
rf_model2_predict_class = predict(rf_model2, test, type = 'class')
rf_model2_predict_score = predict(rf_model2, test, type = 'prob')

# Create Confusion Matrix for all the four models
conf_mat_model1 = table(test$Personal_Loan, model1_predict_class)
conf_mat_model1

conf_mat_model2 = table(test$Personal_Loan, model2_predict_class)
conf_mat_model2

conf_mat_rf_model1 = table(test$Personal_Loan, rf_model1_predict_class)
conf_mat_rf_model1

conf_mat_rf_model2 = table(test$Personal_Loan, rf_model2_predict_class)
conf_mat_rf_model2

# Accuracy of models on test data
accuracy_model1 = sum(diag(conf_mat_model1)) / sum(conf_mat_model1)

accuracy_model2 = sum(diag(conf_mat_model2)) / sum(conf_mat_model2)

accuracy_rf_model1 = sum(diag(conf_mat_rf_model1)) / sum(conf_mat_rf_model1)

accuracy_rf_model2 = sum(diag(conf_mat_rf_model2)) / sum(conf_mat_rf_model2)

# Sensitivity of models on test data
sensitivity_model1 = conf_mat_model1[2,2] / sum(conf_mat_model1['1',])

sensitivity_model2 = conf_mat_model2[2,2] / sum(conf_mat_model2['1',])

sensitivity_rf_model1 = conf_mat_rf_model1[2,2] / sum(conf_mat_rf_model1['1',])

sensitivity_rf_model2 = conf_mat_rf_model2[2,2] / sum(conf_mat_rf_model2['1',])

# Specificity of models on test data
specificity_model1 = conf_mat_model1[1,1] / sum(conf_mat_model1['0',])

specificity_model2 = conf_mat_model2[1,1] / sum(conf_mat_model2['0',])

specificity_rf_model1 = conf_mat_rf_model1[1,1] / sum(conf_mat_rf_model1['0',])

specificity_rf_model2 = conf_mat_rf_model2[1,1] / sum(conf_mat_rf_model2['0',])

# Precision of models on test data
precision_model1 = conf_mat_model1[2,2] / sum(conf_mat_model1[,'1'])

precision_model2 = conf_mat_model2[2,2] / sum(conf_mat_model2[,'1'])

precision_rf_model1 = conf_mat_rf_model1[2,2] / sum(conf_mat_rf_model1[,'1'])

precision_rf_model2 = conf_mat_rf_model2[2,2] / sum(conf_mat_rf_model2[,'1'])

# Using library ROCR functions prediction and performance
pred_model1 = prediction(model1_predict_score[, 2], test$Personal_Loan) 
perf_model1 = performance(pred_model1,"tpr","fpr")
ks_model1 = max(attr(perf_model1,'y.values')[[1]] - attr(perf_model1,'x.values')[[1]])

pred_model2 = prediction(model2_predict_score[, 2], test$Personal_Loan) 
perf_model2 = performance(pred_model2,"tpr","fpr")
ks_model2 = max(attr(perf_model2,'y.values')[[1]] - attr(perf_model2,'x.values')[[1]])

pred_rf_model1 = prediction(rf_model1_predict_score[, 2], test$Personal_Loan) 
perf_rf_model1 = performance(pred_rf_model1,"tpr","fpr")
ks_rf_model1 = max(attr(perf_rf_model1,'y.values')[[1]] - attr(perf_rf_model1,'x.values')[[1]])

pred_rf_model2 = prediction(rf_model2_predict_score[, 2], test$Personal_Loan) 
perf_rf_model2 = performance(pred_rf_model2,"tpr","fpr")
ks_rf_model2 = max(attr(perf_rf_model2,'y.values')[[1]] - attr(perf_rf_model2,'x.values')[[1]])

# Using library ROCR
auc_model1 = performance(pred_model1, measure = "auc")
auc_model1 = auc_model1@y.values[[1]]

auc_model2 = performance(pred_model2, measure = "auc")
auc_model2 = auc_model2@y.values[[1]]

auc_rf_model1 = performance(pred_rf_model1, measure = "auc")
auc_rf_model1 = auc_rf_model1@y.values[[1]]

auc_rf_model2 = performance(pred_rf_model2, measure = "auc")
auc_rf_model2 = auc_rf_model2@y.values[[1]]

# Using library ineq 
gini_model1 = ineq(model1_predict_score[, 2],"gini")

gini_model2 = ineq(model2_predict_score[, 2],"gini")

gini_rf_model1 = ineq(rf_model1_predict_score[, 2],"gini")

gini_rf_model2 = ineq(rf_model2_predict_score[, 2],"gini")

concordance_model1 = Concordance(actuals = ifelse(test$Personal_Loan == '1', 1,0), predictedScores = ifelse(model1_predict_class == '1', 1,0))

concordance_model2 = Concordance(actuals = ifelse(test$Personal_Loan == '1', 1,0), predictedScores = ifelse(model2_predict_class == '1', 1,0))

concordance_rf_model1 = Concordance(actuals = ifelse(test$Personal_Loan == '1', 1,0), predictedScores = ifelse(rf_model1_predict_class == '1', 1,0))

concordance_rf_model2 = Concordance(actuals = ifelse(test$Personal_Loan == '1', 1,0), predictedScores = ifelse(rf_model2_predict_class == '1', 1,0))


model1_metrics = c(accuracy_model1, sensitivity_model1, specificity_model1, precision_model1, ks_model1, auc_model1, gini_model1, concordance_model1$Concordance)

model2_metrics = c(accuracy_model2, sensitivity_model2, specificity_model2, precision_model2, ks_model2, auc_model2, gini_model2, concordance_model2$Concordance)

rf_model1_metrics = c(accuracy_rf_model1, sensitivity_rf_model1, specificity_rf_model1, precision_rf_model1, ks_rf_model1, auc_rf_model1, gini_rf_model1, concordance_rf_model1$Concordance)

rf_model2_metrics = c(accuracy_rf_model2, sensitivity_rf_model2, specificity_rf_model2, precision_rf_model2, ks_rf_model2, auc_rf_model2, gini_rf_model2, concordance_rf_model2$Concordance)

comparison_table = data.frame(model1_metrics, model2_metrics, rf_model1_metrics, rf_model2_metrics)

rownames(comparison_table) = c("Accuracy", "Sensitivity", "Specificity", "Precision", "KS", "Auc", "Gini", "Concordance")

comparison_table

#======================================================================= 
# 
# T H E - E N D 
# 
#=======================================================================

# Generate the .R file from this .Rmd to hold the source code 

purl("Thera Bank Project.Rmd", documentation = 0)
