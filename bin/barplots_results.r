setwd(".")
options(stringsAsFactors = FALSE)
cat("\014")
set.seed(11)

list.of.packages <- c("easypackages", "ggplot2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library("easypackages")
libraries(list.of.packages)

#source("./confusion_matrix_rates.r")
source("./utils.r")


# Hepatitis C prediction on the discovery cohort

# +0.810,   0.828,       0.959,       0.750,      0.991,	 0.923,     0.964   &      0.915  &       0.972 \\
# +0.771    & 0.785    &       0.954         & 0.680 &          0.993	&	0.927 &         0.957 &        0.711 &        0.836\\

discovery_cohort_predictions <- c(0.81, 0.771, 0.828, 0.785, 0.959, 0.954, 0.75, 0.68, 0.991, 0.993, 0.923, 0.927, 0.964, 0.957, 0.915, 0.711, 0.972, 0.836)
test_title <- "Hepatitis_C_prediction"
myPlotTitle <- "hepatitis C prediction on the discovery cohort"

# # Cirrhosis prediction on the validation cohort
# # #
# # +0.275  &    0.765    &      0.680    &      0.812    &      0.444   &	 0.722   &      0.571   &      0.753    &   0.632 \\
# # +0.037 & 0.675             & 0.569        & 0.663     &       0.375 &         0.688 &         0.349 &         0.685 & 0.519
# # 
# 
# 
validation_cohort_predictions <- c(0.275, 0.037, 0.765, 0.675, 0.680, 0.569, 0.812, 0.663, 0.444, 0.375, 0.722, 0.688, 0.571, 0.349, 0.753, 0.685, 0.632, 0.519)
test_title <- "Cirrhosis_prediction"
myPlotTitle <- "cirrhosis prediction on the validation cohort"

# selected_predictions <- discovery_cohort_predictions
selected_predictions <- validation_cohort_predictions

results_dataframe <- data.frame(
                methods=rep(c(" Two-features RF", "AST/ALT ratio"),9),
                rates=rep(c("MCC", "F1", "accuracy", "TPR", "TNR", "PPV", "NPV", "PR AUC", "ROC AUC"), each=2), 
                values=selected_predictions
                )
                
num_to_return <- 1
upper_num_limit <- 1000
exe_num <- sample(1:upper_num_limit, num_to_return)

#  results_dataframe$"method" <- factor(results_dataframe$"method", level=c(1:length(results_dataframe$"method")))

print(results_dataframe)
                
# head(results_dataframe)
# 
# method rate         value
#1 RF              MCC       0.810 
#2 criterion   MCC      0.771
#3 RF               F1         0.828
#4 criterion   F1          0.785
#5 RF               accuracy   0.959
#6 criterion   accuracy    0.954
#7 RF               TPR    0.750
#8 criterion   TPR     0.680
#9 RF               TNR    0.991
#10 criterion   TNR     0.993
#11 RF               PPV    0.923
#12 criterion   PPV    0.927
#13 RF              NPV   0.964
#14 criterion   NPV   0.957
#15 RF              PRAUC   0.915
#16 criterion   PRAUC   0.711
#17 RF              ROCAUC  0.972
#18 criterion   ROCAUC  0.836

myTextSize <- 8

p <- ggplot(data=results_dataframe, aes(x=rates, y=values, fill=methods)) + geom_bar(stat="identity", position=position_dodge()) +
  scale_x_discrete(limits=results_dataframe$"rates") + ggtitle(myPlotTitle) + theme(text=element_text(size=myTextSize), axis.text=element_text(size=myTextSize), axis.title=element_text(size=myTextSize), plot.title = element_text(hjust = 0.5)) + 
  ylim(0, 1)

p

plot_height <- 6

general_file <- paste("../plots/", test_title, "_rand", exe_num,  ".pdf", sep="")
SAVE_GENERAL_PLOT <- TRUE
    
if (SAVE_GENERAL_PLOT) {
      ggsave(p, file=general_file, height = plot_height, width = 16, units = "cm", dpi = 150)
      cat("saved file: ", general_file, "\n",  sep="")
}

computeExecutionTime()
