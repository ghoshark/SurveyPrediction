library(plumber)
R <- plumber::plumb("C:/Data_Science/SurveyPrediction/R/plumber_survey.R")

# The explicit mention of host is to ensure the API can be called from external machines too
R$run(host="0.0.0.0",port = 8000, swagger = TRUE)

