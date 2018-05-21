# Install & Load Packages
# p <- c("readr","tidyr","dplyr","ggplot2","rpart","lattice","rpart.plot","caret","lubridate","ggplot2","party","randomForest","rattle","RColorBrewer")
# install.packages(p)
# sapply(p, library, character.only = TRUE)
# https://www.edureka.co/blog/implementation-of-decision-tree/

#' @title survey
#' @description This function return incidents
#' @aliases survey
#' @param filenames "C:/Data_Science/Text_Analysis/IPO_R&D_Raw.xlsx"
#' @return NULL
#' @examples survey("C:/Data_Science/Text_Analysis/IPO_R&D_Raw.xlsx")
#' @name survey
#' @export

survey <- function(filenames) {
  # clear all objects
  rm(list = ls())
  #read the incidents file
  # we know that file has some missing values as "--" and "", so replacing them with NA while importing
  if (file.exists(filenames)) {
    survey <- read.csv(filenames,header = TRUE, na.strings = c("--",""))
  }
  else  {
    survey <- readxl::read_xlsx("C:/Data_Science/SurveyPrediction/IPO_R&D_Raw.xlsx",sheet = "Survey Raw data", col_names = TRUE)
      }

#Remove spaces from column names
#names(survey)
names(survey) <- make.names(names(survey))

#Pick only relavant columns
survey1 <- data.frame(survey$Ticket.Country,survey$Ticket.Assigned.Group,survey$Product.Name,survey$Category.Tier1,survey$Category.Tier3, survey$Satisfaction.Rating )
#Change the datatype to character and merge the values
survey1$survey.Satisfaction.Rating <- as.character(survey1$survey.Satisfaction.Rating)

#Merge the Satisfied and Very Satisfied values to simply Satisfied
survey1$survey.Satisfaction.Rating[survey1$survey.Satisfaction.Rating %in% c("Very Satisfied","Satisfied")] <- "SATISFIED"
survey1$survey.Satisfaction.Rating[survey1$survey.Satisfaction.Rating %in% c("Very Dissatisfied","Dissatisfied")] <- "DISSATISFIED"
#Change it back to a factor
survey1$survey.Satisfaction.Rating <- as.factor(survey1$survey.Satisfaction.Rating)

#Find and omit all rocords with "NA"
summary(survey1)
# Find NA s columnwise in the dataset
sapply(survey1, function(x) sum(is.na(x)))
# Find NAs rowwise
rowSums(is.na(survey1))
survey1 <- na.omit(survey1)

# Save the cleaned dataset
write.csv(survey1,file="survey1.csv", row.names = TRUE)

#partition dataset into training and test datasets
#Option 1 to partition data
indexes = sample(1:nrow(survey1), size=0.8*nrow(survey1))
train <- survey1[indexes,]
test <- survey1[-indexes,]

#Decision Tree Model; Satisfaction Rating, ctree gives the levels not found error
#survey_model <- party::ctree(survey.Satisfaction.Rating ~ survey.Ticket.Country+survey.Ticket.Region+survey.Ticket.Assigned.Group+survey.Product.Name+survey.Category.Tier1+survey.Category.Tier2+survey.Category.Tier3, data=survey1)
survey_model <- rpart::rpart(survey.Satisfaction.Rating ~ survey.Ticket.Country+survey.Ticket.Assigned.Group+survey.Product.Name+survey.Category.Tier1+survey.Category.Tier3, data=survey1, method = "class")
# rattle::fancyRpartPlot(survey_model_pruned, uniform=TRUE, main="Pruned Classification Tree")
save(survey_model, file="data/survey_model.rda")

# Optimize the Decision Tree by pruning

# Validation of Tree using 'Complexity Parameter'
# This function provides the optimal prunings based on the cp value.
# We prune the tree to avoid any overfitting of the data. The convention is to have a small tree and the one with least cross validated error given by printcp() function i.e. ‘xerror’.
rpart::printcp(survey_model)
# From the above mentioned list of cp values, we can select the one having the least cross-validated error and use it to prune the tree.\
# The value of cp should be least, so that the cross-validated error rate is minimum.
survey_model$cptable[which.min(survey_model$cptable[,"xerror"]),"CP"]
# plotcp() provides a graphical representation to the cross validated error summary.
# The cp values are plotted against the geometric mean to depict the deviation until the minimum value is reached
rpart::plotcp(survey_model)
# Prune the tree to create an optimal decision tree
survey_model_pruned <- rpart::prune(survey_model,cp= survey_model$cptable[which.min(survey_model$cptable[,"xerror"]),"CP"])
# rattle::fancyRpartPlot(survey_model_pruned, uniform=TRUE, main="Pruned Classification Tree")
save(survey_model_pruned, file="data/survey_model_pruned.rda")

# Test the prediction model and check accuracy
# plot(survey_model)
# rattle::fancyRpartPlot(survey_model)
# print(summary(survey_model))
# Note that without type="class" it gives a 2 column output with the probablities for Satisifed and Dissatisfied.
# survey.Satisfaction.Rating_P <- predict(survey_model_pruned, newdata = test, type="class")
# result <- data.frame(test,survey.Satisfaction.Rating_P)
# xtab <- table(test$survey.Satisfaction.Rating,survey.Satisfaction.Rating_P)
# caret::confusionMatrix(xtab)

return(NULL)

}
