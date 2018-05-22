library(plumber)

serve_model <- plumb("R/plumber.R")
serve_model$run(port = 8000)

