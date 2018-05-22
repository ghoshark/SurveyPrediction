library(plumber)

#Load the current RDA files
load(file = "data/survey_model_pruned.rda")
load(file = "data/survey_model.rda")

MODEL_VERSION <- "0.0.1"
VARIABLES <- list(
  survey.Ticket.Country = "Ticket Country",
  survey.Ticket.Assigned.Group = "survey Ticket Assigned Group",
  survey.Product.Name = "Product Name",
  survey.Category.Tier1 = "Category Tier1",
  survey.Category.Tier3 = "Category.Tier3",
  survey.Satisfaction.Rating = "Successful submission will result in a calculated customer satisfaction Probability of either SATISFIED or DISSATIAFIED"
)

#' @get /Healthcheck
health_check <- function() {
  result <- data.frame(
    "input" = "",
    "status" = 200,
    "model_version" = MODEL_VERSION
  )

  return(result)
}

# A Simple Landing Page
#' @get /Homepage
#' @html
home <- function() {
  title <- "Survey Prediction API"
  body_intro <-  "Welcome to the Survey Prediction API!"
  body_model <- paste("We are currently serving model version:", MODEL_VERSION)
  body_msg <- paste("To receive a prediction on Customer Satisfaction,",
                    "submit the following variables to the <b>/survival</b> endpoint:",
                    sep = "\n")
  body_reqs <- paste(VARIABLES, collapse = "<br>")

  result <- paste(
    "<html>",
    "<h1>", title, "</h1>", "<br>",
    "<body>",
    "<p>", body_intro, "</p>",
    "<p>", body_model, "</p>",
    "<p>", body_msg, "</p>",
    "<p>", body_reqs, "</p>",
    "</body>",
    "</html>",
    collapse = "\n"
  )

  return(result)
}

validate_input <- function(survey.Ticket.Country, survey.Ticket.Assigned.Group, survey.Product.Name,survey.Category.Tier1,survey.Category.Tier3) {

survey.Ticket.Country.VALID <- (survey.Ticket.Country %in% c(
    "United States",
    "Czech Republic",
    "India",
    "Germany",
    "France",
    "Spain",
    "Mexico",
    "Singapore",
    "China",
    "Japan",
    "Russian Federation",
    "Italy",
    "Australia",
    "Denmark",
    "Brazil",
    "Canada",
    "Morocco",
    "Belgium",
    "Indonesia",
    "Latvia",
    "Turkey",
    "Saudi Arabia",
    "Laos",
    "Viet Nam",
    "Sweden",
    "New Zealand",
    "Hungary",
    "Thailand",
    "Philippines",
    "Austria",
    "United Kingdom",
    "Pakistan",
    "Chile",
    "Taiwan, Province Of China",
    "Korea, Republic Of",
    "Slovakia",
    "Serbia",
    "Poland",
    "Colombia",
    "Oman"))
  survey.Ticket.Assigned.Group.VALID <- (survey.Ticket.Assigned.Group %in% c(
                           "ANALYTICS_WW_L2",
                           "SYMPHONY_WW_L2",
                           "MASTERDATA_WW_L3",
                           "FJTU_SERVICE_DESK_RD_L2",
                           "PHENIX_WW_L3",
                           "PIM_SUPPORT_WW_L1",
                           "SYMPHONY_ADMIN_WW_L1",
                           "MCAD_WW_L2",
                           "USER_ADMIN_CHINA_CC_L1",
                           "SPOT_WW_L2",
                           "CADENCE_WW_L2",
                           "DOCEMEDIA_ADMIN_FR_L2",
                           "CREO_VIEW_WW_L3",
                           "CASE_WW_L2",
                           "SCAN_WW_L2",
                           "SYMPHONY_WW_L3",
                           "TEMPO_WW_L3",
                           "USER_ADMIN_IN_CC_L1",
                           "CADENCE_WW_L3",
                           "USER_ADMIN_EMEA_CC_L1",
                           "EFILE_WW_L2",
                           "USER_ADMIN_MX_CC_L1",
                           "AUTOCAD_WW_L2",
                           "ETO_TMA_WW_L2",
                           "TEMPO_WW_L2",
                           "MOTIF_WW_L2",
                           "GEOMAGIC_WW_L2",
                           "GENESIS_WW_L2",
                           "VALOR_WW_L3" ))
  survey.Product.Name.VALID <-  (survey.Product.Name %in% c("Flux 3D",
                                                          "Symphony",
                                                          "Inventor",
                                                          "Tempo",
                                                          "Phenix",
                                                          "Other",
                                                          "Genesis",
                                                          "PDM",
                                                          "ProEngineer",
                                                          "Autocad",
                                                          "Autocad - 2016",
                                                          "SPoT",
                                                          "PTC Creo Parametric",
                                                          "eFile",
                                                          "Network Drive",
                                                          "DocEmedia",
                                                          "PTC CreoView",
                                                          "Viewer",
                                                          "Git Repo CLI client",
                                                          "Autocad - Mechanical",
                                                          "TeamForge",
                                                          "Cadence",
                                                          "Squoring",
                                                          "TeamForge Full",
                                                          "TeamForge Contributor",
                                                          "ODM Sys Admin",
                                                          "Autocad - Electrical",
                                                          "Phoenix Cubes",
                                                          "Microsoft Visual Studio Professional",
                                                          "Coda",
                                                          "Tango",
                                                          "Windchill",
                                                          "Autocad - ETO",
                                                          "Allegro PCB Designer",
                                                          "ANSYS",
                                                          "Software Tools",
                                                          "Galaxy",
                                                          "Flux 2D",
                                                          "TeamForge Editor",
                                                          "Microsoft Visual Studio Test Pro",
                                                          "Autocad Applications",
                                                          "Microsoft Visual Studio Enterprise",
                                                          "Other Software",
                                                          "Autodesk - Revit",
                                                          "Revit MEP",
                                                          "Opus",
                                                          "PSPICE Capture CIS",
                                                          "MOTIF-Arbortext",
                                                          "Geomagic",
                                                          "ETO",
                                                          "On/Offboarding",
                                                          "Outlook" ))

  survey.Category.Tier1.VALID <- (survey.Category.Tier1 %in% c("Request",
                                                       "Troubleshoot",
                                                       "Reset"))

  survey.Category.Tier3.VALID <- (survey.Category.Tier3 %in% c("License",
                                                             "Application Functionality",
                                                             "Administration",
                                                             "Installation",
                                                             "n/a",
                                                             "Role Management",
                                                             "Functionality",
                                                             "Issue",
                                                             "Other",
                                                             "ODM Plus",
                                                             "Activate",
                                                             "Account",
                                                             "Detailing/Drawing",
                                                             "Access",
                                                             "Product View",
                                                             "PIM Launcher",
                                                             "Exports",
                                                             "Group Rename",
                                                             "Password",
                                                             "Document Management",
                                                             "Dwg True View",
                                                             "Consolidated UIS",
                                                             "Eng Dwg Viewer",
                                                             "Folder Management",
                                                             "Login",
                                                             "Production Issue",
                                                             "Intralink/MDM",
                                                             "Deactivate",
                                                             "Autocad Electrical",
                                                             "Loading Data",
                                                             "Other Tools",
                                                             "Order Viewer",
                                                             "Project",
                                                             "3D Modeling and Assembly",
                                                             "Interfaces (Coda, ProE, ERP, Other)",
                                                             "Group Creation",
                                                             "OE Tools",
                                                             "Editing",
                                                             "ProE Crash/Trial Files/Fatal Errors",
                                                             "Viewing and Printing"
  ))

  test_results <- c(survey.Ticket.Country.VALID, survey.Ticket.Assigned.Group.VALID, survey.Product.Name.VALID,survey.Category.Tier1.VALID,survey.Category.Tier3.VALID)
  if(!all(test_results)) {
    failed <- which(!test_results)
    return(test_results[failed])
  } else {
    return("OK")
  }
}

transform_input <- function(survey.Ticket.Country, survey.Ticket.Assigned.Group, survey.Product.Name,survey.Category.Tier1,survey.Category.Tier3) {
  survey.Ticket.Country <- as.factor(tolower(survey.Ticket.Country))
  survey.Ticket.Assigned.Group <- as.factor(c(survey.Ticket.Assigned.Group))
  survey.Product.Name <- as.factor(tolower(survey.Product.Name))
  survey.Category.Tier1 <- as.factor(tolower(survey.Category.Tier1))
  survey.Category.Tier3 <- as.factor(tolower(survey.Category.Tier3))
}

#' @get /SurveyPrediction
#' @html
#' @param survey.Ticket.Country
#' @param survey.Ticket.Assigned.Group
#' @param survey.Product.Name
#' @param survey.Category.Tier1
#' @param survey.Category.Tier3

predict_survey <- function(survey.Ticket.Country, survey.Ticket.Assigned.Group, survey.Product.Name,survey.Category.Tier1,survey.Category.Tier3)
  {
  survey.Ticket.Country = survey.Ticket.Country
  survey.Ticket.Assigned.Group = survey.Ticket.Assigned.Group
  survey.Product.Name = survey.Product.Name
  survey.Category.Tier1 = survey.Category.Tier1
  survey.Category.Tier3 = survey.Category.Tier3

  valid_input <- validate_input(survey.Ticket.Country, survey.Ticket.Assigned.Group, survey.Product.Name,survey.Category.Tier1,survey.Category.Tier3)
  if (valid_input == "OK")  {
  transform_input(survey.Ticket.Country, survey.Ticket.Assigned.Group, survey.Product.Name,survey.Category.Tier1,survey.Category.Tier3)
  newdata <- data.frame(survey.Ticket.Country, survey.Ticket.Assigned.Group, survey.Product.Name,survey.Category.Tier1,survey.Category.Tier3)

   #incident_model is included with the package
   newdata$survey.Satisfaction.Rating <- as.vector(predict(survey_model_pruned, newdata = newdata, type="class"))
   newdata$survey.Satisfaction.Probability <- predict(survey_model_pruned, newdata = newdata, type="prob")
   #pruned models are having some issues - the numbers don't change for any combination of input- need to check
   newdata$survey.Satisfaction.Rating <- as.vector(predict(survey_model, newdata = newdata, type="class"))
   newdata$survey.Satisfaction.Probability <- predict(survey_model, newdata = newdata, type="prob")

   #Write the result output to a csv file
   write.csv(newdata,file="C:/Data_Science/SurveyPrediction/output.csv", row.names = FALSE)

   Satisfaction.Rating <- paste("Satisfaction Rating",newdata$survey.Satisfaction.Rating,sep=": ")
   sat_prob <- gsub("[[:space:]]", "", paste(round(newdata$survey.Satisfaction.Probability[2] *100),"%"))
   dissat_prob <- gsub("[[:space:]]", "", paste(round(newdata$survey.Satisfaction.Probability[1] *100),"%"))
   Satisfaction.Probability <- paste("Satisfaction Probability",sat_prob,sep=": ")
   Dissatisfaction.Probability <- paste("Dissatisfaction Probability",dissat_prob,sep=": ")
   result <- paste(Satisfaction.Probability,Dissatisfaction.Probability,Satisfaction.Rating,sep= "\n")
   #result <- newdata$survey.Satisfaction.Probability[2]
  }

  else {
    result <- list(
    input = list(survey.Ticket.Country, survey.Ticket.Assigned.Group, survey.Product.Name,survey.Category.Tier1,survey.Category.Tier3),
    response = list(input_error = valid_input),
    status = 400,
    model_version = MODEL_VERSION)
  }
   return(result)
}
