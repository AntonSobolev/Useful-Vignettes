setwd("/Users/sobol/Documents/GitHub/Useful-Vignettes/Automated-Scripts-croneR")

if (!require("pacman")) install.packages("pacman")
p_load(data.table, dplyr, methods, keras, gbm, caret, pROC, devtools)
#install_keras()
p_load(cronR,miniUI,shiny, shinyFiles )

# devtools::install_github("bnosac/cronR")
# 
# install.packages('miniUI')
# install.packages('shiny')
# install.packages('shinyFiles')

smth <- "hello world"


write.table(smth, "new-results.csv")
