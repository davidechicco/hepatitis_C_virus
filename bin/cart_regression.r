setwd(".")
options(stringsAsFactors = FALSE)
cat("\014")
set.seed(11)

NUMBER_OF_EXECUTIONS <- 100

fileName <- "../data/hcvdat0_EDITED.csv"
targetName <- "category_0healthy_1hepatitis_2fibrosis_3cirrhorsis"
MISSING_DATA_IMPUTATION <- TRUE


# fileName <- "../data/dataset_edited_without_time.csv"
# targetName <- "death_event"

# fileName <- "../../../projects/sepsis_severity_ICU/data/sepsis_severity_dataset_edited_2019-02-11.csv"
# targetName <- "ADDED.survival"

cat("fileName: ", fileName, "\n", sep="")
cat("targetName: ", targetName, "\n", sep="")

list.of.packages <- c("easypackages", "clusterSim", "PRROC", "e1071", "rpart",  "dplyr", "pastecs",  "Metrics", "MLmetrics", "mice")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library("easypackages")
libraries(list.of.packages)

source("./confusion_matrix_rates.r")
source("./utils.r")

threshold <- 0.5

# file reading
patients_data <- read.csv(fileName, header = TRUE, sep =",");
cat("Read data from file ", fileName, "\n", sep="")

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

target_index <- dim(patients_data)[2]
original_patients_data <- patients_data

execution_number <- NUMBER_OF_EXECUTIONS
cat("Number of executions = ", execution_number, "\n", sep="")
for(exe_i in 1:execution_number)
{

    cat(">>> execution number: ", exe_i, "\n", sep="")

    # shuffle the rows
    patients_data <- patients_data[sample(nrow(patients_data)),] 

    # Allocation of the size of the training set
    perce_training_set <- 80
    size_training_set <- round(dim(patients_data)[1]*(perce_training_set/100))

    cat("perce_training_set = ",perce_training_set,"%\n", sep="")

    # Allocation of the training set and of the test set
    training_set <- (patients_data[1:size_training_set,])
    test_set_index_start <- size_training_set+1
    test_set_index_end <- dim(patients_data)[1]
    test_set  <- patients_data[test_set_index_start:test_set_index_end,]

    test_labels <- patients_data[test_set_index_start:test_set_index_end, target_index]   # NEW


    cat("dim(training_set) ")
    cat(dim(training_set), "\n")

    cat("dim(test_set) ")
    cat(dim(test_set), "\n")


    # Generation of the CART model
    allFeaturesFormula <- as.formula(paste(as.factor(colnames(patients_data)[target_index]), '.', sep=' ~ ' ))
    cart_model <- rpart(allFeaturesFormula, method="class", data=training_set);

    pred_test_predictions <- as.numeric(predict(cart_model, test_set, typ="class"))-1
    
    
    thisResultMat <- regression_rates(test_labels, pred_test_predictions, "@@@ Test set @@@")

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

printResultsLatex("CART", meanSigmaRowResults)

computeExecutionTime()