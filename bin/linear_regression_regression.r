setwd(".")
options(stringsAsFactors = FALSE)
cat("\014")
set.seed(11)

fileName <- "../data/hcvdat0_EDITED.csv"
targetName <- "category_0healthy_1hepatitis_2fibrosis_3cirrhorsis"
MISSING_DATA_IMPUTATION <- TRUE



cat("fileName: ", fileName, "\n", sep="")
cat("targetName: ", targetName, "\n", sep="")

list.of.packages <- c("easypackages", "PRROC", "e1071", "randomForest","class", "gmodels", "formula.tools", "dplyr", "pastecs", "mice")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library("easypackages")
libraries(list.of.packages)


source("./confusion_matrix_rates.r")
source("./utils.r")

threshold <- 0.5

patients_data <- read.csv(file=fileName,head=TRUE,sep=",",stringsAsFactors=FALSE)
cat("fileName = ", fileName, "\n", sep="")

patients_data$"category_0healthy_1sick" <- NULL

if(MISSING_DATA_IMPUTATION==TRUE){

    # missing data imputation
    NUM_DATASETS <- 1
    imputed_data <- mice(patients_data, m=NUM_DATASETS, maxit = 50, method = 'pmm', seed = 500)
    patients_data <- complete(imputed_data, NUM_DATASETS)
}


NUM_METRICS <- 5
resultDataFrame <- matrix(ncol=NUM_METRICS, nrow=1)
colnames(resultDataFrame) <- c("R^2", "RMSE", "MAE", "MSE", "SMAPE")

# let's put the target label last on the right 
patients_data <- patients_data%>%select(-targetName,targetName)
patients_data_original <- patients_data

# cycle of executions

execution_number <- 100
cat("Number of executions = ", execution_number, "\n", sep="")
for(exe_i in 1:execution_number)
{

    cat("[Execlution number ", exe_i, " out of ", execution_number, "]\n", sep="" )
    patients_data <- patients_data[sample(nrow(patients_data)),] # shuffle the rows

    target_index <- dim(patients_data)[2]
    cat("target_index = ", target_index, "\n", sep="") 

    training_set_perce <- 80

    cat("training_set_perce = ", training_set_perce, "%\n", sep="")

    # the training set is the first training_set_perce% of the whole dataset
    training_set_first_index <- 1 # NEW
    training_set_last_index <- round(dim(patients_data)[1]*training_set_perce/100) # NEW

    # the test set is the last 20% of the whole dataset
    test_set_first_index <- round(dim(patients_data)[1]*training_set_perce/100)+1 # NEW
    test_set_last_index <- dim(patients_data)[1] # NEW

    cat("[Creating the subsets for the values]\n")
    prc_data_train <- patients_data[training_set_first_index:training_set_last_index, 1:(target_index-1)] # NEW
    prc_data_test <- patients_data[test_set_first_index:test_set_last_index, 1:(target_index-1)] # NEW


    cat("[Creating the subsets for the labels \"1\"-\"0\"]\n")
    prc_data_train_labels <- patients_data[training_set_first_index:training_set_last_index, target_index] # NEW
    prc_data_test_labels <- patients_data[test_set_first_index:test_set_last_index, target_index]   # NEW

    library(class)
    library(gmodels)

    # apply k-NN with k_best to the test set

    cat("\n[Training the linear regression model on training set & applying the linear regression to test set]\n", sep="")

    lin_reg_model_new <- lm(prc_data_train_labels ~ ., data=prc_data_train)
    pred_test_predictions <- as.numeric(predict(lin_reg_model_new, prc_data_test))
    
    thisResultMat <- regression_rates(prc_data_test_labels, pred_test_predictions, "@@@ Test set @@@")

     if (exe_i == 1)  resultDataFrame <-  thisResultMat
    else  resultDataFrame <- rbind(resultDataFrame, thisResultMat)
    
 }
 
cat("\n\n\n=== final results ===\n")
 
 cat("Number of executions = ", execution_number, "\n", sep="")
 # statistics on the dataframe of confusion matrices
 statDescConfMatr <- stat.desc(resultDataFrame)
meanSigmaRowResults <- (statDescConfMatr)[c("mean", "std.dev"),]
cat("\n\n")
print(dec_three(meanSigmaRowResults))
cat("\n\n=== === === ===\n")

printResultsLatex("Linear regression", meanSigmaRowResults)

computeExecutionTime()
