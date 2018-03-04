setwd("") #working dir of the files
library(plumber)
r <- plumb("businessmap.R")
r$run(port=8000)
