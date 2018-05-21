#' @title Survey Score
#' @description This function return the satisfaction rating prediction
#' @aliases survey_score
#' @param survey.Ticket.Country India
#' @param survey.Ticket.Assigned.Group SYMPHONY_WW_L2
#' @param survey.Product.Name Symphony
#' @param survey.Category.Tier1 Troubleshoot
#' @param survey.Category.Tier3 License
#' @return data frame
#' @examples
#'  input <- data.frame(
#'  survey.Ticket.Country = "India",
#'  survey.Ticket.Assigned.Group="SYMPHONY_WW_L2",
#'  survey.Product.Name="Symphony",
#'  survey.Category.Tier1="Troubleshoot",
#'  survey.Category.Tier3="License"
#'  )
#'  OR
#'  input = "C:/Data_Science/SurveyPrediction/input.csv"
#'  SurveyPrediction::survey_score(input)
#' @name survey_score
#' @export
  survey_score <- function(input){
  #input can either be csv file or data
  newdata <- if(is.character(input) && file.exists(input)){
    read.csv(input)
  } else {
    as.data.frame(input)
  }

  stopifnot("survey.Ticket.Country" %in% names(newdata))
  stopifnot("survey.Ticket.Assigned.Group" %in% names(newdata))
  stopifnot("survey.Product.Name" %in% names(newdata))
  stopifnot("survey.Category.Tier1" %in% names(newdata))
  stopifnot("survey.Category.Tier3" %in% names(newdata))

  newdata$survey.Ticket.Country <- as.factor(newdata$survey.Ticket.Country)
  newdata$survey.Ticket.Assigned.Group <- as.factor(newdata$survey.Ticket.Assigned.Group)
  newdata$survey.Product.Name <- as.factor(newdata$survey.Product.Name)
  newdata$survey.Category.Tier1 <- as.factor(newdata$survey.Category.Tier1)
  newdata$survey.Category.Tier3 <- as.factor(newdata$survey.Category.Tier3)

  #Load the current RDA files
  load(file = "data/survey_model_pruned.rda")
  load(file = "data/survey_model.rda")

  #incident_model is included with the package
  newdata$survey.Satisfaction.Rating <- as.vector(predict(survey_model_pruned, newdata = newdata, type="class"))
  #Write the result output to a csv file
  write.csv(newdata,file="C:/Data_Science/SurveyPrediction/output.csv", row.names = FALSE)
  return(newdata)
}
