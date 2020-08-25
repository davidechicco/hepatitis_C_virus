setwd(".")
options(stringsAsFactors = FALSE)
cat("\014")
set.seed(18)

# agregateTwoSortedRankings
agregateTwoSortedRankings <- function(dd, firstColumnName, secondColumnName) {

    cat("\n[function agregateTwoSortedRankings()]\n")

    # dd_sorted_MSE <- dd[order(-dd$firstColumnName), ]
    dd_sorted_firstColumn <- dd[order(-dd[[firstColumnName]]), ]
    # print(dd_sorted_firstColumn)
    
    dd_sorted_secondColumn <- dd[order(-dd[[secondColumnName]]), ]
    # print(dd_sorted_IncNodePurity);


    # varImpPlot(rf_output)
    dd_sorted_firstColumn_only <- dd_sorted_firstColumn
    dd_sorted_firstColumn_only[[secondColumnName]] <- NULL # we do not need the other values
    dd_sorted_firstColumn_only$firstColPos <- c(1:dim(dd_sorted_firstColumn_only)[1])
    
    dd_sorted_secondColumn_only <- dd_sorted_secondColumn
    dd_sorted_secondColumn_only[[firstColumnName]] <- NULL # we do not need the other values
    dd_sorted_secondColumn_only$secondColPos <- c(1:dim(dd_sorted_secondColumn_only)[1])

    dd_sorted_firstColumn_only$features <- rownames(dd_sorted_firstColumn_only)
    dd_sorted_secondColumn_only$features <- rownames(dd_sorted_secondColumn_only)

    # let's sort alphabetically
    dd_sorted_firstColumn_only <- dd_sorted_firstColumn_only[order(dd_sorted_firstColumn_only$"features"), ]
    dd_sorted_secondColumn_only <- dd_sorted_secondColumn_only[order(dd_sorted_secondColumn_only$"features"), ]
    
    
    cat("\ncbind()\n")
    mergedRanking <- cbind(dd_sorted_firstColumn_only, dd_sorted_secondColumn_only)

    mergedRankingAlphaBeta <- mergedRanking[order(mergedRanking$"features"), ]
    mergedRankingAlphaBeta$posSum <- mergedRankingAlphaBeta$firstColPos + mergedRankingAlphaBeta$secondColPos

    mergedRankingGeneralRank <- mergedRankingAlphaBeta[order(mergedRankingAlphaBeta$"posSum"), ]
    mergedRankingGeneralRank$finalPos <- c(1:dim(mergedRankingGeneralRank)[1])
    
    # remove duplicate columns
    temp <- mergedRankingGeneralRank[, !duplicated(colnames(mergedRankingGeneralRank))]
    mergedRankingGeneralRank <- temp

    # print(mergedRankingGeneralRank)
    
    return (mergedRankingGeneralRank)

}


# EXP_ARG_NUM <- 2
# 
# args = commandArgs(trailingOnly=TRUE)
# if (length(args)<EXP_ARG_NUM) {
#   stop("At least two argument must be supplied (input files)", call.=FALSE)
# } else {
#   # default output file
#   fileNameData <- args[1]
#   targetName <- args[2]
# }



fileNameData <- "../data/hcvdat0_EDITED_fibrosis_cirrhosis.csv"
targetName <- "category_0fibrosis_1cirrhosis"
MISSING_DATA_IMPUTATION <- TRUE

# fileNameData <- "../data/journal.pone.0118297_S1_Dataset_HPV_EDITED_cirrhosis.csv"
# targetName <- "cirrhosis"
# MISSING_DATA_IMPUTATION <- TRUE

list.of.packages <- c("easypackages", "randomForest", "ggplot2", "dplyr", "mice")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library("easypackages")
libraries(list.of.packages)

source("./confusion_matrix_rates.r")
source("./utils.r")


FEATURE_RANKING_PLOT_DEPICTION <-  FALSE
TWO_FEATURES_PLOT <- FALSE

patients_data <- read.csv(fileNameData, header = TRUE, sep =",");
cat("Read data from file ", fileNameData, "\n", sep="")

# rename target
names(patients_data)[names(patients_data) == targetName] <- "target"

cat("application of dplyr::select()\n")
patients_data <- patients_data%>%dplyr::select(-target,target)
target_index <- dim(patients_data)[2]    


# patients_data$"HCV.RNATaqman.Log.IU.ml." <- as.numeric(patients_data$"HCV.RNATaqman.Log.IU.ml.")

# patients_data$"category_0healthy_1hepatitis_2fibrosis_3cirrhorsis" <- NULL
if(MISSING_DATA_IMPUTATION==TRUE){

    # missing data imputation
    NUM_DATASETS <- 1
    imputed_data <- mice(patients_data, m=NUM_DATASETS, maxit = 50, method = 'pmm', seed = 500)
    patients_data <- complete(imputed_data, NUM_DATASETS)
}


TRAINING_SET_RATIO <- 0.95
TEST_SET_RATIO <- 1 - TRAINING_SET_RATIO

patients_training_set_index_start <- 1
patients_training_set_index_end <- round(nrow(patients_data) * TRAINING_SET_RATIO)
patients_test_set_index_start <- patients_training_set_index_end + 1
patients_test_set_index_end <- nrow(patients_data)

patients_training_set <- patients_data[patients_training_set_index_start:patients_training_set_index_end,]
patients_test_set <- patients_data[patients_test_set_index_start:patients_test_set_index_end,]


num_to_return <- 1
upper_num_limit <- 10000000
exe_num <- sample(1:upper_num_limit, num_to_return)


allExecutionsFinalRanking <- data.frame(Doubles=double(),
                 Ints=integer(),
                 Factors=factor(),
                 Logicals=logical(),
                 Characters=character(),
                 stringsAsFactors=FALSE)

execution_number <- 100 
cat("Number of executions = ", execution_number, "\n", sep="")
for(exe_i in 1:execution_number)
{

    cat("\n\n\n Execution number ", exe_i,"\n", sep="")
    cat("[Randomizing the rows]\n")
    patients_training_set <- patients_training_set[sample(nrow(patients_training_set)),] # shuffle the rows


    cat("application of randomForest()\n")
    rf_output <- randomForest(as.factor(patients_training_set$target) ~ ., data=patients_training_set, importance=TRUE, proximity=TRUE)
        

    dd <- as.data.frame(rf_output$importance);
    
    mergedRankingGeneralRank <- agregateTwoSortedRankings(dd, "MeanDecreaseAccuracy", "MeanDecreaseGini")
    
    rownames(mergedRankingGeneralRank) <- (removeDot(removeUnderscore(rownames(mergedRankingGeneralRank))))
    mergedRankingGeneralRank$features <- removeDot(removeUnderscore(mergedRankingGeneralRank$features))

    print(mergedRankingGeneralRank[, c("finalPos", "MeanDecreaseAccuracy", "MeanDecreaseGini"), drop=FALSE])

    finalRankingOneExecution <- mergedRankingGeneralRank[, c("features", "finalPos", "MeanDecreaseAccuracy", "MeanDecreaseGini"), drop=FALSE]
    finalRankingOneExecutionAlphaBeta <- finalRankingOneExecution[order(finalRankingOneExecution$"features"), , drop=FALSE]

    if (exe_i == 1) {
        allExecutionsFinalRanking <- finalRankingOneExecutionAlphaBeta
    } else {
        
        allExecutionsFinalRanking$MeanDecreaseAccuracy <- allExecutionsFinalRanking$MeanDecreaseAccuracy + finalRankingOneExecutionAlphaBeta$MeanDecreaseAccuracy
        allExecutionsFinalRanking$MeanDecreaseGini <- allExecutionsFinalRanking$MeanDecreaseGini + finalRankingOneExecutionAlphaBeta$MeanDecreaseGini
        allExecutionsFinalRanking$finalPos <- allExecutionsFinalRanking$finalPos + finalRankingOneExecutionAlphaBeta$finalPos
    }
}



allExecutionsFinalRanking$MeanDecreaseAccuracy <- allExecutionsFinalRanking$MeanDecreaseAccuracy / execution_number
allExecutionsFinalRanking$MeanDecreaseGini <- allExecutionsFinalRanking$MeanDecreaseGini / execution_number
allExecutionsFinalRanking$finalPos <- allExecutionsFinalRanking$finalPos / execution_number

# # let's eliminate the target index from the rank
# targetRow <-  which(allExecutionsFinalRanking==targetName)
# allExecutionsFinalRanking <- allExecutionsFinalRanking[-c( which(allExecutionsFinalRanking==targetName)), ]

cat("\n\n\n\n== final ranking after ", execution_number, " executions == \n", sep="")

allExecutionsFinalRanking_mse_Gini <-  allExecutionsFinalRanking[, c("MeanDecreaseAccuracy", "MeanDecreaseGini")]
aggregateRankings <- agregateTwoSortedRankings(allExecutionsFinalRanking_mse_Gini, "MeanDecreaseAccuracy", "MeanDecreaseGini")

# print(aggregateRankings[, c("finalPos", "MeanDecreaseAccuracy", "MeanDecreaseGini")])

print(allExecutionsFinalRanking_mse_Gini[order(-allExecutionsFinalRanking_mse_Gini["MeanDecreaseAccuracy"]), ])


top_features_num <- 2
selectedFeaturesNames <- rownames((allExecutionsFinalRanking_mse_Gini[order(-allExecutionsFinalRanking_mse_Gini["MeanDecreaseAccuracy"]), ])[1:top_features_num,])

cat("number of selected top features: ", top_features_num, "\n", sep="")
cat("selected top features: ", selectedFeaturesNames[1],  " and ", selectedFeaturesNames[2], "\n", sep="")


patients_training_set_reduced_features <- patients_training_set[, selectedFeaturesNames]
patients_test_set_reduced_features <- patients_test_set[, selectedFeaturesNames]

cat("\n[Training Random Forests classifier on the test set with only the top ", top_features_num ," features]\n")
rf_new <- NULL
allFeaturesFormula <- as.formula(paste(as.factor(colnames(patients_data)[target_index]), '.', sep=' ~ ' ))
rf_new <- randomForest(allFeaturesFormula, data=patients_training_set, importance=TRUE, proximity=TRUE)
    
cat("\n[Applying the trained Random Forests classifier on the test set with only the top ", top_features_num ," features]\n")
patients_data_test_PRED <- predict(rf_new, patients_test_set, type="response")
patients_data_test_labels <- patients_test_set$target
thisConfMat <- confusion_matrix_rates(patients_data_test_labels, patients_data_test_PRED, "@@@ Test set @@@")
