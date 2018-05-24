library(plumber)

server_model <- plumb("R/plumber.R")
# the explicit mention of host is to ensure the API can be called fom external machines also
server_model$run(host="0.0.0.0",port = 8000)


